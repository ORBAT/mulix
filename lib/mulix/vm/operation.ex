defmodule Op.Record do
  require Record
  Record.defrecord(:oper, :Op, name: :nop, op: &Quark.id/1, parens: nil)

  defmacro __using__(_opts) do
    quote do
      require Record
      Record.defrecordp(:oper, :Op, name: :nop, op: &Quark.id/1, parens: nil)
    end
  end
end

defmodule Op do
  defmodule BadReturnError do
    defexception message: "bad return type", value: :not_set
  end

  @moduledoc ~S"""
  An `Op` is an operation that uses and possibly modifies an `Environment` by e.g. manipulating
  stacks.

  Create new instances with `new/2`.

  `Op`s have the following fields:

  * `name :: atom()`: a human-readable name for the operation.

  A `Record` macro for creating new `Op`s is defined with the name `oper`:

      use Op
      oper(fn x -> x end, :my_op_name)
  """

  import Op.Record

  @type t ::
          record(
            :oper,
            name: atom(),
            op: Op.op() | Type.expr(),
            parens: nil | integer()
          )

  @typedoc """
  Take an `Environment` and return a possibly modified `Environment`
  """
  @type op :: (Environment.t() -> Environment.t())

  @doc """
  Create a new `Op`. If `active` is false, the returned op will be a no-op
  """
  def new(op, name, parens \\ nil), do: oper(name: name, op: op, parens: parens)

  @doc """
  Gets the actual function of an `Op`. Useful for tests
  """
  defmacro get_op(op) do
    quote do
      unquote(op) |> oper(:op)
    end
  end

  def get_parens(oper(parens: parens)) do
    parens || 0
  end

  def get_parens(_expr) do
    0
  end

  @doc false
  defmacro __using__(_opts) do
    quote do
      import Op
      @before_compile Op
      Module.register_attribute(__MODULE__, :op_names, accumulate: true)
      Module.register_attribute(__MODULE__, :extra_inlines, accumulate: true)
    end
  end

  @doc false
  defmacro __before_compile__(_env) do
    quote do
      op_list =
        for op_name <- @op_names do
          {op_name, 0}
        end

      # @op_tuple_list op_list

      to_be_inlined =
        for kw <- @extra_inlines |> Enum.uniq(),
            into: op_list do
          kw
        end

      # IO.inspect(
      #   [op_list: op_list, extra_inlines: @extra_inlines |> Enum.uniq()],
      #   label: "#{__MODULE__} __before_compile__"
      # )

      @dialyzer {:nowarn_function, op_list}

      @compile {:inline, to_be_inlined}

      def op_names do
        @op_names
      end

      @spec ops() :: [Op.t()]
      def ops() do
        for name <- @op_names do
          apply(__MODULE__, name, [])
        end
      end

      @spec ops([atom()]) :: [Op.t()]
      def ops(only) do
        for name <- @op_names, Enum.member?(only, name) do
          apply(__MODULE__, name, [])
        end
      end
    end
  end

  @doc """
  Defines an `Op` that takes its input from multiple stacks, and possibly changes multiple stacks.

  In the body, the variable `env` will be bound to the `Environment`.

  If the operation body evaluates to a map, it will be used as the replacement stack map for the
  `Environment`. If the body evaluates to an `Environment` struct, it'll be used as the replacement
  env. If it evaluates to `nil`, the environment will not be modified at all.

  `kw` is a list of keyword arguments. Supported arguments:

  * `in`. Takes a keyword list `[stack_name: match_pattern, ...]` where `stack_name` a stack name
    and `match_pattern` is the pattern to use for matching that stack. The variables will be bound
    in the operator body. Example that expects at least 2 items on the `:exec` stack (but doesn't
    bind them), and one item on the `:boolean` stack:
    ```
    defmultiop :if, in: [exec: [_b, _a | _execrest], boolean: [c | boolrest]],  do: [...]
    ```

  * `parens`: number of paren groups this op needs. Optional, will be deducted from `:exec` pattern
  if not set.

  """
  @spec defmultiop(atom(), [in: [{Stack.name(), list()}], parens: non_neg_integer()], do: any()) ::
          any()
  defmacro defmultiop(name, kw, do: body) do
    quote generated: true,
          bind_quoted: [
            name: Macro.escape(name, unquote: true),
            kw: Macro.escape(kw, unquote: true),
            body: Macro.escape(body, unquote: true)
          ] do
      op_name =
        case name do
          {atom, _, nil} ->
            atom

          # TODO(ORBAT): this'll probably break shit
          other ->
            other
        end

      @op_names op_name

      inputs = hd(Keyword.get_values(kw, :in))
      pats = {:%{}, [], inputs}

      parens =
        case Keyword.get(kw, :parens, nil) do
          nil -> length(Keyword.get(inputs, :exec, []))
          other -> other
        end

      def unquote(op_name)() do
        Op.new(
          fn
            var!(env) = %Environment{stacks: unquote(pats)} ->
              case result = unquote(body) do
                nil -> var!(env)
                result = %Environment{} -> result
                result = %{} -> Environment.with_stacks(var!(env), result)
                other -> raise(BadReturnError, value: other)
              end

            env ->
              env
          end,
          unquote(op_name),
          unquote(parens)
        )
      end
    end
  end

  defp defop_ast(name, stack, pattern, body) do
    quote generated: true,
          bind_quoted: [
            name: Macro.escape(name, unquote: true),
            stack: Macro.escape(stack, unquote: true),
            pattern: Macro.escape(pattern, unquote: true),
            body: Macro.escape(body, unquote: true)
          ] do
      op_name =
        case name do
          {atom, _, nil} ->
            atom

          atom when is_atom(atom) ->
            atom
        end

      @op_names op_name
      def unquote(op_name)() do
        Op.new(
          fn
            env = %Environment{stacks: %{unquote(stack) => unquote(pattern)}} ->
              env |> Environment.update_stack(unquote(stack), unquote(body))

            env ->
              env
          end,
          unquote(op_name)
        )
      end
    end
  end

  @doc false
  defmacro gen_input_stack_def(op_name, input_stacks, default_inp_stack) do
    quote generated: true,
          bind_quoted: [
            op_name: Macro.escape(op_name, unquote: true),
            input_stacks: Macro.escape(input_stacks, unquote: true),
            default_inp_stack: Macro.escape(default_inp_stack, unquote: true)
          ] do
      @spec input_stack_for(atom(), [Stack.name()]) :: Stack.name()

      # Given an op name atom and a `:stack` stack, returns the input stack for the op based on the
      # current stack (top of `stack_stack`) and the stacks the op is defined for (`input_stack` when
      # defining). If the current stack doesn't match, uses either the `default_inp_stack` for the op,
      # or the global default default stack if none set.

      defp input_stack_for(op_name, stack_stack)

      # Given an op name, returns the input stacks it is defined for, or `[]` if it has no
      # restrictions.
      defp input_stack_for(unquote(op_name)), do: unquote(input_stacks)

      case input_stacks do
        [_ | _] ->
          # op has stack restrictions. If the current stack is any of the stacks on the list, return
          # that stack
          for stack <- input_stacks do
            defp input_stack_for(unquote(op_name), [unquote(stack) | _]) do
              unquote(stack)
            end
          end

        [] ->
          # no stack restrictions, so always return current stack
          defp input_stack_for(unquote(op_name), [curr_stack | _]), do: curr_stack
      end

      # if none of the above match, return the default stack
      defp input_stack_for(unquote(op_name), _), do: unquote(default_inp_stack)
    end
  end

  @doc false
  defmacro gen_output_stack_defp(op_name, output_stack) do
    quote generated: true,
          bind_quoted: [
            op_name: Macro.escape(op_name, unquote: true),
            output_stack: Macro.escape(output_stack, unquote: true)
          ] do
      defp output_stack_for(unquote(op_name)), do: unquote(output_stack)

      case output_stack do
        # output stack not specified, so always use the input stack
        nil ->
          defp output_stack_for(unquote(op_name), stacks),
            do: input_stack_for(unquote(op_name), stacks)

        # output stack was a list. If the stack below current matches, return that stack
        stack_list = [_ | _] ->
          for stack <- stack_list do
            defp output_stack_for(unquote(op_name), [_, unquote(stack) | _]) do
              unquote(stack)
            end
          end

          # default to the first stack in `output_stack` if nothing matches
          defp output_stack_for(unquote(op_name), _stacks),
            do: unquote(hd(output_stack))

        # output stack is an atom, so always output to that stack
        stack when is_atom(stack) ->
          defp output_stack_for(unquote(op_name), _stacks), do: unquote(stack)
      end
    end
  end

  @doc false
  defmacro gen_no_output_stack_op(op_name, pattern, body) do
    quote generated: true,
          bind_quoted: [
            op_name: Macro.escape(op_name, unquote: true),
            pattern: Macro.escape(pattern, unquote: true),
            body: Macro.escape(body, unquote: true)
          ] do
      def unquote(op_name)() do
        Op.new(
          fn
            var!(env) = %Environment{stacks: %{stack: stack_stack}} ->
              var!(input_stack) = input_stack_for(unquote(op_name), stack_stack)

              case(var!(env).stacks) do
                %{^var!(input_stack) => unquote(pattern)} ->
                  var!(env) |> Environment.update_stack(var!(input_stack), unquote(body))

                _ ->
                  var!(env)
              end

            env ->
              env
          end,
          unquote(op_name)
        )
      end
    end
  end

  @doc false
  defmacro gen_output_stack_op(op_name, pattern, body) do
    quote generated: true,
          bind_quoted: [
            op_name: Macro.escape(op_name, unquote: true),
            pattern: Macro.escape(pattern, unquote: true),
            body: Macro.escape(body, unquote: true)
          ] do
      num_drops =
        case pattern do
          list when is_list(list) -> length(pattern)
          _ -> 0
        end

      def unquote(op_name)() do
        Op.new(
          fn
            var!(env) = %Environment{stacks: stacks = %{stack: stack_stack}} ->
              var!(input_stack) = input_stack_for(unquote(op_name), stack_stack)
              var!(output_stack) = output_stack_for(unquote(op_name), stack_stack)
              e = var!(env)

              case e.stacks do
                %{^var!(input_stack) => unquote(pattern)} ->
                  var!(env)
                  |> Environment.drop(var!(input_stack), unquote(num_drops))
                  |> Environment.push(var!(output_stack), unquote(body))

                _ ->
                  var!(env)
              end

            env ->
              env
          end,
          unquote(op_name)
        )
      end
    end
  end

  @doc """
  Defines an `Op` that takes its source and destination stacks from the `:stack` stack. It will
  operate on the current stack (stack on top of the `:stack` stack), or a default stack if the
  current stack isn't compatible. stack restrictions can be defined, see below.

  If an output stack is *not* specified, the body of the op will be used as a replacement for the
  input stack. If an output stack *is* specified, the body of the op will be *pushed* to the output
  stack, and as many arguments as were matched on the input stack will be dropped (so an `input_pattern`
  of `[a, b, c | rest]` with an `output_stack` specified will lead to 3 pops in the input).

  The argument `kw` is a keyword list of the following:

  - `input_stack`: an optional stack name or list of stack names for which the op is defined. The
    current stack will be used if it matches any of these. If not, `default_inp_stack` is used.

  - `default_inp_stack`: default input stack to use if either `input_stack` is empty, or the current
    stack doesn't match it

  - `output_stack`: optional output stack to use. Will *push* output into the output stack. If
    `output_stack` is just an atom like `:code`, the output is always pushed to that stack. If it is
    a list of atoms (`[:number, :code]`), the item *below* the current stack's name (stack `name:
    (output_stack input_stack -- )`) will be used if it matches any of these. If the `:stack` stack
    doesn't have enough elements or the 2nd from top doesn't match anything in the list, the first
    item in `output_stack` will be used as the default output stack.
  """
  defmacro defgenericop(name, input_pattern, kw \\ [], do: body) do
    quote generated: true,
          bind_quoted: [
            name: Macro.escape(name, unquote: true),
            kw: Macro.escape(kw, unquote: true),
            pattern: Macro.escape(input_pattern, unquote: true),
            body: Macro.escape(body, unquote: true)
          ] do
      op_name =
        case name do
          {atom, _, nil} ->
            atom

          atom when is_atom(atom) ->
            atom
        end

      input_stacks =
        case Keyword.get(kw, :input_stack, []) do
          atom when is_atom(atom) -> [atom]
          other -> other
        end

      default_inp_stack = Keyword.get(kw, :default_inp_stack, Default.Types.default_stack())
      output_stack = Keyword.get(kw, :output_stack, nil)

      # IO.inspect([pattern: pattern], label: "#{op_name} macro")

      @op_names op_name

      case output_stack do
        # no output stack specified, use input
        nil ->
          Op.gen_no_output_stack_op(
            unquote(op_name),
            unquote(pattern),
            unquote(body)
          )

        # output stack specified
        _ ->
          Op.gen_output_stack_op(
            unquote(op_name),
            unquote(pattern),
            unquote(body)
          )
      end

      Op.gen_input_stack_def(unquote(op_name), unquote(input_stacks), unquote(default_inp_stack))

      Op.gen_output_stack_defp(unquote(op_name), unquote(output_stack))
    end
  end

  defmacro defop(name, stack, input_pattern, do: body) do
    defop_ast(name, stack, input_pattern, body)
  end
end

defmodule Op.Stacks do
  use Op

  #  defop_new :zot, :code, [a | rest] do
  #    [rest]
  #  end

  defgenericop :len, inp_stack,
    default_inp_stack: :code,
    output_stack: :number,
    binds: [:input_stack] do
    # IO.inspect([input_stack: input_stack], label: "len")
    length(inp_stack)
  end

  @doc ~S"""
  Pops the input stack and pushes item to `:return`. No-op if input is empty
  """
  defgenericop :pop_ret, [a | _], default_inp_stack: :code, output_stack: :return do
    a
  end

  defgenericop :dup, [a | rest] do
    [a, a | rest]
  end

  defgenericop :swap, [a, b | rest] do
    [b, a | rest]
  end

  defgenericop :over, [a, b | rest] do
    [a, b, a | rest]
  end

  defgenericop :drop, [_ | rest] do
    rest
  end

  defgenericop :rot, [c, b, a | rest] do
    [a, c, b | rest]
  end

  defgenericop :nip, [_, b | rest] do
    [b | rest]
  end
end

defmodule Op.Generic do
  use Op
  require Type

  defmacrop xor(a, b) do
    quote do
      (unquote(a) or unquote(b)) and not (unquote(a) and unquote(b))
    end
  end

  def safe_num_parse(str) when is_binary(str) do
    fun =
      if String.contains?(str, [<<?.>>, <<?E>>]) do
        &Float.parse/1
      else
        &Integer.parse/1
      end

    case fun.(str) do
      :error -> 0
      {num, _} -> num
    end
  end

  def safe_num_parse(_), do: 0

  def bool_to_num(true), do: -1
  def bool_to_num(false), do: 0

  def add_items(b_type, item, prev)

  def add_items(Type.number(), a, b), do: a + b
  def add_items(Type.string(), a, b), do: <<a::bitstring, b::bitstring>>

  def add_items(Type.boolean(), a, b), do: xor(a, b)

  def add_items(Type.code(), item, prev) when is_list(prev), do: [item | prev]

  def add_items(Type.code(), item, prev), do: [item, prev]

  @extra_inlines {:add_items, 3}

  # TODO(ORBAT): defgenericop param that allows having multiple input and output types?

  defmultiop :generic_add, parens: 0, in: [stack: [a_stack, b_stack | _]] do
    case env.stacks do
      %{^a_stack => [a_top | a_rest], ^b_stack => [b_top | b_rest]} ->
        a_top_type = env.stack_to_type[a_stack]
        b_top_type = env.stack_to_type[b_stack]

        maybe_converted_a_top =
          cond do
            a_top_type != b_top_type -> a_top |> Type.Convert.to(b_top_type)
            true -> a_top
          end

        %{
          env.stacks
          | a_stack => a_rest,
            b_stack => [add_items(b_top_type, maybe_converted_a_top, b_top) | b_rest]
        }

      _ ->
        env.stacks
    end
  end
end

defmodule Op.Number do
  use Op

  defop :add, :number, [a, b | rest] do
    [a + b | rest]
  end

  defop :sub, :number, [a, b | rest] do
    [a - b | rest]
  end

  defop :mul, :number, [a, b | rest] do
    [a * b | rest]
  end

  defop :div, :number, [a, b | rest] do
    [a / b | rest]
  end

  defop :neg, :number, [a | rest] do
    [-a | rest]
  end
end

defmodule Op.Boolean do
  use Op

  defmacrop xor(a, b) do
    quote do
      (unquote(a) or unquote(b)) and not (unquote(a) and unquote(b))
    end
  end

  defop :not, :boolean, [a | rest] do
    [not a | rest]
  end

  defop :and, :boolean, [a, b | rest] do
    [a and b | rest]
  end

  defop :or, :boolean, [a, b | rest] do
    [a or b | rest]
  end

  defop :xor, :boolean, [a, b | rest] do
    [xor(a, b) | rest]
  end
end

defmodule Op.Type do
  use Op
end

defmodule Op.Tag do
  use Op

  defmultiop :tag, parens: 0, in: [stack: [from_stack | _]] do
    case env.stacks do
      %{^from_stack => [top | rest]} ->
        env |> Environment.update_stack(from_stack, rest) |> Environment.tag(top)

      _ ->
        env
    end
  end

  defmultiop :get_tagged, parens: 0, in: [stack: [from_stack | _]] do
    case env.stacks do
      %{^from_stack => [top | rest]} ->
        env_popped = env |> Environment.update_stack(from_stack, rest)
        tagged = env_popped |> Environment.get_tagged(top)
        env_popped |> Environment.push(:exec, tagged)

      _ ->
        env
    end
  end

  # defgenericop :read_tag, [a | _],
  #   output_stack: [
  #     :type_boolean,
  #     :type_code,
  #     :type_name,
  #     :type_number,
  #     :type_string,
  #     :type_type
  #   ] do
  #   env |> Environment.get_tagged(a)
  # end
end

defmodule Op.Code do
  use Op

  defmultiop :code_quote, in: [exec: [a | exec], code: code] do
    %{
      env.stacks
      | :exec => exec,
        :code => [a | code]
    }
  end
end

defmodule Op.Exec do
  @moduledoc """
  Operators for that deal with the `:exec` stack (or execution in general)
  """
  use Op

  @doc """
  A no-op. Does nothing.
  """
  def noop(), do: Op.new(fn env -> env end, :noop)

  @extra_inlines {:noop, 0}

  @doc """
  The Y-combinator. Used to implement recursion. `exec: ( a -- a y a )`
  """
  defop :y, :exec, [a | rest] do
    [a, y(), a | rest]
  end

  @doc """
  'if' operator. Runs either the top or the second from top exec expression depending on bool stack
  top.

  Expects stacks to be:
      bool ( val )
      exec ( a b )

  If `val` is true, run `b`, else run `a`.

  IOW `val == true` will nip exec (`( a b -- b )`), otherwise it'll drop (`( a b -- a)`)

  """
  defmultiop :if, in: [exec: [_b, _a | _execrest], boolean: [c | boolrest]] do
    stack_op = if c, do: &Stack.nip/1, else: &Stack.drop/1
    exec = env |> Environment.stack_op(:exec, stack_op)

    %{
      env.stacks
      | :exec => exec,
        :boolean => boolrest
    }
  end
end
