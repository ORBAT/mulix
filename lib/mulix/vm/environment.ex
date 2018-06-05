# Environmentin voisi lykätä Stateen.
# Operaatiot on funkkareita, op(Environment.stacks() :: Environment.stacks())
#

###### EI STACKEJA OLLENKAAN?
## http://benjamintan.io/blog/2015/02/05/how-to-build-streams-in-elixir-easily-with-stream-resource-awesomeness/
##
## jokainen yksilö lukee streamista stream(A) sisään
##
## operaatiot: A -> B
## - stream transform
## -

defmodule Environment.Attribs do
  @moduledoc """
  `Environment.Attribs` defines a struct and convenience functions for
  """
  import Algae

  @doc """
  Creates a new `Environment.Attribs` with the keys and values in the keyword list `kw_list`.
  Anything not in the list is set to the default
  """
  def new(kw_list) when is_list(kw_list) do
    struct(__MODULE__, kw_list)
  end

  @typedoc """
  A `t:rand_expr_gen/0` is a wrapper tuple for functions that take an `Attribs` and turn them into
  random expressions. `name` is the name of the generator
  """
  @type rand_expr_gen :: {name :: atom(), fun :: (Environment.Attribs.t() -> Type.expr())}

  @dialyzer {[:no_contracts, :no_return], [Enum.map(0..16, &{:new, &1})]}
  defdata do
    # The default maximum number of steps allowed per execution. One instruction or literal is one
    # step, and descending into a list (so each `[]`) is one step.
    evalpush_limit :: integer() \\ Default.evalpush_limit()

    # When TRUE (which is the default), code passed to the top level of the interpreter will be
    # pushed onto the CODE stack prior to execution.
    # TODO(ORBAT): implement
    top_level_push_code :: boolean() \\ true

    # When TRUE, the CODE stack will be popped at the end of top level calls to the interpreter. The
    # default is FALSE.
    # TODO(ORBAT): implement
    top_level_pop_code :: boolean() \\ false

    # Names available in the environment, but not necessarily defined.
    # Should be a superset of `defined_names`
    available_names :: %{atom() => Type.expr()} \\ %{}

    # Ops available in the environment
    available_ops :: %{atom() => Op.t()} \\ %{}

    # A list of functions that can generate random expressions.
    # TODO(ORBAT): use in at least µsh. Maybe provide something like Push's `CODE.RAND`?
    expr_generators :: list(rand_expr_gen())

    # The minimum INTEGER that will be produced as an ephemeral random INTEGER constant or from a
    # call to INTEGER.RAND.
    # TODO(ORBAT): implement
    min_random_integer :: integer() \\ -:math.pow(2, 63)

    # The maximum INTEGER that will be produced as an ephemeral random INTEGER constant or from a
    # call to INTEGER.RAND.
    # TODO(ORBAT): implement
    max_random_integer :: integer() \\ :math.pow(2, 63) - 1

    # The minimum FLOAT that will be produced as an ephemeral random FLOAT constant or from a call
    # to FLOAT.RAND.
    # TODO(ORBAT): implement
    min_random_float :: integer() \\ -1

    # The maximum FLOAT that will be produced as an ephemeral random FLOAT constant or from a call
    # to FLOAT.RAND.
    # TODO(ORBAT): implement
    max_random_float :: integer() \\ 1

    # The maximum number of points in an expression produced by the CODE.RAND instruction.
    # TODO(ORBAT): is this useful in Mu?
    max_points_in_random_expressions :: integer()

    # The maximum number of points that can occur in any program on the CODE stack. Instructions
    # that would violate this limit act as NOOPs (they do nothing).
    # TODO(ORBAT): implement
    max_points_in_program :: integer() \\ Default.max_points()

    # How likely it is that a gene will be silenced
    silence_probability :: float() \\ 0.02

    # The probability that the selection of the ephemeral random NAME constant for inclusion in
    # randomly generated code will produce a new name (rather than a name that was previously
    # generated).
    # TODO(ORBAT): implement
    new_erc_name_probability :: float()

    # A seed for the random number generator
    # TODO(ORBAT): implement
    random_seed :: integer()

    # Genome-specific attributes
    genome_attribs :: %{module => any()}
  end

  @doc """
  Get the value of an attribute.
  """
  defmacro get_attrib(env, name) do
    quote do
      unquote(env).attribs.unquote(name)
    end
  end

  @doc """
    Expands to %Environment{attribs: %Environment.Attribs{unquote(name) => unquote(val)}}.

    Handy in e.g. function heads.
  """
  defmacro match_attrib(name, val) do
    quote do
      %Environment{attribs: %Environment.Attribs{unquote(name) => unquote(val)}}
    end
  end

  @doc """
  Sets the attribute named `name` to `val`. Result can be assigned to get the modified env:
      new_env = env |> set_attrib(:evalpush_limit, 900)
  """
  defmacro set_attrib(env, name, val) do
    quote do
      attr = unquote(env).attribs

      %Environment{
        unquote(env)
        | attribs: %Environment.Attribs{attr | unquote(name) => unquote(val)}
      }
    end
  end

  defmacro get_genome_attrib(env, genome, name) do
    quote do
      (unquote(env) |> get_attrib(unquote(genome)))[unquote(name)]
    end
  end

  defmacro set_genome_attrib(env, genome, name, value) do
    quote do
      updated_attrib_map =
        unquote(env) |> get_attrib(unquote(genome)) |> Map.put(unquote(name), unquote(value))

      unquote(env) |> set_attrib(unquote(genome), updated_attrib_map)
    end
  end

  defmacro __using__(_opts) do
    quote do
      import Environment.Attribs, except: unquote(Enum.map(0..16, &{:new, &1}))
    end
  end
end

defmodule Environment do
  @moduledoc """
  An `Environment` is the execution environment for µlix programs. It supports multiple different
  "evaluators" (see `Evaluator` module) that define how expressions are evaluated.

  It contains multiple stacks that are operated on by operations (like `Op` or `Genome`), and
  attributes (in `Environment.Attribs`) like the maximum number of program points or maximum number
  of steps per execution.

  If the execution of one step results in an error on the `:error` stack, the interpreter aborts
  immediately, leaving its stacks in the states they were in prior to the abort (so they may still
  be examined by a calling program).
  """

  import Op.Record

  use Environment.Attribs
  use InliningTools

  # @type t :: %Environment{stacks: list()}

  @type stacks() :: %{Stack.name() => Stack.t(any())}

  # @default_op_modules [Op.Number, Op.Exec, Op.Stacks, Op.Code]
  # def default_op_modules, do: @default_op_modules

  @doc """
  Create a new `Environment`. Defaults to execution stacks and defined names.
  """

  @type t :: %Environment{
          evaluator: module(),
          stacks: stacks(),
          stack_to_type: %{Stack.name() => Type.t()},
          type_to_stacks: %{Type.t() => [Stack.name()]},
          defined_names: %{atom() => Type.expr()},
          attribs: Environment.Attribs.t(),
          tagged: GBTree.t()
        }

  defstruct evaluator: nil,
            defined_names: %{},
            stacks: %{},
            stack_to_type: %{},
            type_to_stacks: %{},
            attribs: %Environment.Attribs{},
            tagged: GBTree.new()

  @type stack_to_type :: %{Stack.name() => Type.t()}

  @spec new(
          module,
          stack_to_type,
          %{atom => Type.expr()},
          Environment.Attribs.t()
        ) :: Environment.t()
  @dialyzer {[:no_contracts, :no_return], [new: 0, new: 1, new: 2, new: 3, new: 4]}
  def new(
        evaluator \\ Evaluator.Mu,
        stack_to_type_map \\ Default.Types.phenome_stack_to_type(),
        defined_names \\ Default.defined_names_map(),
        attribs \\ %Environment.Attribs{}
      ) do
    %Environment{
      evaluator: evaluator,
      defined_names: defined_names,
      stacks: Default.Types.to_stacks(stack_to_type_map),
      stack_to_type: stack_to_type_map,
      type_to_stacks: Default.Types.type_to_stacks_map(stack_to_type_map),
      attribs: attribs
    }
  end

  def get_name(env, atom) when is_atom(atom) do
    env.defined_names[atom]
  end

  @inlines {:get_name, 2}

  @spec get_stacks(Environment.t(), [Stack.name()]) :: stacks()
  def get_stacks(%Environment{stacks: sts}, stack_names), do: sts |> Map.take(stack_names)

  @spec get_stacks(Environment.t()) :: stacks()
  def get_stacks(%Environment{stacks: sts}), do: sts

  def get_stack(env, stack_name) do
    env.stacks[stack_name]
  end

  @inlines {:get_stacks, 2}

  @spec update_stack(Environment.t(), Stack.name(), Stack.t()) :: Environment.t()
  @doc """
  Replaces the stack `stack_name` with `stack`.
  """
  def update_stack(env = %Environment{stacks: sts}, stack_name, stack) do
    %Environment{env | stacks: sts |> Map.replace!(stack_name, stack)}
  end

  @inlines {:update_stack, 3}

  def with_stacks(env, stack_map) do
    %Environment{env | stacks: stack_map}
  end

  @inlines {:with_stacks, 2}

  def add_stack(env = %Environment{stacks: sts}, stack_name, init_contents \\ []) do
    %Environment{env | stacks: sts |> Map.put(stack_name, init_contents)}
  end

  @inlines {:add_stack, 2}
  @inlines {:add_stack, 3}

  def add_name(env, op = oper(name: name)) do
    env |> add_name(name, op)
  end

  def add_name(env = %Environment{defined_names: names}, name, expr) do
    %Environment{env | defined_names: Map.put(names, name, expr)}
  end

  @inlines {:add_name, 2}
  @inlines {:add_name, 3}

  @doc """
  Merges `name_map` with the defined names of the environment. Keys in `name_map` will overwrite
  existing keys.
  """
  def add_names(env = %Environment{defined_names: names}, %{} = name_map) do
    %Environment{env | defined_names: Map.merge(names, name_map)}
  end

  @inlines {:add_names, 2}

  @spec push(Environment.t(), Stack.name(), [any()] | any()) :: Environment.t()
  @doc ~S"""
  Push items onto a stack, so that the last item in a list is the top of the stack.

  `:exec` pushes are done in reverse order, so pushing `[op1, op2, op3]` will result in an exec
  stack with `[op1, op2, op3]` (`op1` being the top). This is done so that expression lists will be
  pushed in the order they should be executed, i.e. so that the first in the list is the first to
  be executed

      # pushing a list into the `:number` stack.
      # 1 is pushed first, then 2 and then 3, leaving 3 at the top
      iex> Environment.new()
      ...> |> Environment.push(:number, [1,2,3])
      ...> |> Environment.get_stack(:number)
      [3, 2, 1]

      # pushing the same list into the `:exec` stack.
      # The first element is pushed last, leaving it at the top
      iex> Environment.new() |> Environment.push(:exec, [1,2,3]) |> Environment.get_stack(:exec)
      [1, 2, 3]
  """
  # exec list pushes need to be handled in reverse order, so push everything in xs first, and then push
  # the head
  def push(env, stack_name, item)

  def push(env, :exec, [x | xs]) do
    env |> push(:exec, xs) |> item_push(:exec, x)
  end

  def push(env, stack_name, [x | xs]) when not is_list(x) do
    env |> push(stack_name, x) |> push(stack_name, xs)
  end

  # if x is a list, push it as-is onto the destination stack
  def push(env, stack_name, [x | xs]) when is_list(x) do
    env |> item_push(stack_name, x) |> push(stack_name, xs)
  end

  def push(env, _stack_name, []), do: env

  def push(env, stack_name, item) do
    item_push(env, stack_name, item)
  end

  @inlines {:push, 3}

  @spec item_push(Environment.t(), Stack.name(), [any()] | any()) :: Environment.t()
  defp item_push(env, stack_name, item) do
    stack = env |> get_stack(stack_name) |> Stack.push(item)
    env |> update_stack(stack_name, stack)
  end

  @doc ~S"""
  Runs a stack operation on the environment.

      iex> Environment.new()
      ...> |> Environment.push(:number, [1,2,3])
      ...> |> Environment.stack_op(:number, &Stack.drop/1)
      [2, 1]
  """
  @spec stack_op(Environment.t(), Stack.name(), (Stack.t() -> Stack.t())) :: Stack.t()
  def stack_op(env, stack_name, op_fun) do
    env |> get_stack(stack_name) |> op_fun.()
  end

  @inlines {:stack_op, 3}

  def env_stack_op(env, stack_name, op_fun) do
    stack = env |> get_stack(stack_name) |> op_fun.()
    env |> update_stack(stack_name, stack)
  end

  @inlines {:env_stack_op, 3}

  def pop(env, stack_name) do
    {item, stack} = env |> get_stack(stack_name) |> Stack.pop()
    {item, env |> update_stack(stack_name, stack)}
  end

  @inlines {:pop, 2}

  def drop(env = %Environment{stacks: stacks}, stack_name) do
    env |> Environment.update_stack(stack_name, Stack.drop(stacks[stack_name]))
  end

  def drop(env = %Environment{stacks: stacks}, stack_name, count) do
    env
    |> Environment.update_stack(
      stack_name,
      Reductions.reduce_times(stacks[stack_name], &Stack.drop/1, count)
    )
  end

  @inlines {:drop, 3}
  @inlines {:drop, 2}

  def clear(env, stack_name), do: env |> update_stack(stack_name, [])
  @inlines {:clear, 2}

  def peek(env, stack_name), do: env |> env.stacks[stack_name]
  @inlines {:peek, 2}

  @doc """
  Pushes `program` onto the `:exec` stack and then executes until `:exec` stack is empty, the
  maximum instruction count is exceeded, or something is added to the `:error` stack.

  If there's an item on the `:error` stack after a step, stacks are reset to the state they were in
  _prior to_ the step.

  From http://faculty.hampshire.edu/lspector/push3-description.html

  ```md
      To execute program P:
        Push P onto the EXEC stack
        LOOP until the EXEC stack is empty:
            If the first item on the EXEC stack is a single instruction
                then pop it and execute it.
            Else if the first item on the EXEC stack is a literal
                then pop it and push it onto the appropriate stack.
            Else (the first item must be a list) pop it and push all of the
                items that it contains back onto the EXEC stack individually,
                in reverse order (so that the item that was first in the list
                ends up on top).
  ```
  """
  def execute(env, program) do
    env |> push(:exec, [program]) |> execute_()
  end

  @inlines {:execute, 2}

  defp execute_(env = %Environment{stacks: %{exec: [_ | _]}}) do
    case env |> step() do
      # if step returns an env with errors on the stack, add them to the *original* env and return
      %__MODULE__{stacks: %{error: errs = [_ | _]}} ->
        env |> update_stack(:error, errs)

      # otherwise, take the returned env and continue execution
      other ->
        other |> execute_()
    end
  end

  defp execute_(env = %Environment{stacks: %{exec: []}}) do
    env
  end

  @doc """
  Executes one step of the `:exec` stack in an `Environment`. Does nothing if the `:exec` stack is
  empty. If `EVALPUSH-LIMIT` would be exceeded by the step, `:evalpush_limit_exceeded` is pushed on
  the `:error` stack.
  """
  def step(env = match_attrib(:evalpush_limit, 0)) do
    env |> push(:error, :evalpush_limit_exceeded)
  end

  def step(env = %Environment{stacks: %{exec: [expr | rest]}}) do
    env_exec_pop =
      env
      |> decrement_eval_limit()
      |> update_stack(:exec, rest)

    env_exec_pop
    |> push(:code, [expr])
    |> env.evaluator.evaluate(expr)
  end

  def step(env = %Environment{stacks: %{exec: []}}) do
    env
  end

  @inlines {:step, 1}

  defp decrement_eval_limit(env = match_attrib(:evalpush_limit, el)) do
    env |> set_attrib(:evalpush_limit, el - 1)
    # %Environment{env | attribs: %Environment.Attribs{attr | evalpush_limit: el - 1}}
  end

  @doc """
  Returns `true` if `env` has a stack with the type `type`, `false` otherwise.
  """
  @spec type_defined?(Environment.t(), Type.t()) :: boolean()
  def type_defined?(env, type), do: Map.has_key?(env.type_to_stacks, type)
  @inlines {:type_defined?, 2}

  @spec stack_defined?(Environment.t(), Stack.name()) :: boolean()
  def stack_defined?(env, stack_name), do: Map.has_key?(env.stacks, stack_name)
  @inlines {:stack_defined?, 2}

  @spec stack_type(Environment.t(), Stack.name()) :: Type.t()
  def stack_type(env, stack_name), do: Map.fetch!(env.stack_to_type, stack_name)
  @inlines {:stack_type, 2}

  def tag(env = %Environment{tagged: tagged}, key, item),
    do: %Environment{env | tagged: tagged |> GBTree.enter(key, item)}

  @inlines {:tag, 3}

  def get_tagged(env, key, nth \\ 0), do: nil
  @inlines {:get_tagged, 2}
  @inlines {:get_tagged, 3}

  #
  #  def get_tagged(%Environment{tagged: tagged}, key, 0),
  #    do: tagged |> GBTree.key_to_node(key)
  #
  #  def get_tagged(%Environment{tagged: tagged}, key, nth),
  #    do: tagged |> GBTree.key_to_nodes(key, nth + 1) |> :lists.reverse() |> hd()
end

defmodule Environment.AttrOps do
  use Op
  import Environment.Attribs

  for attrib <- %Environment.Attribs{} |> Map.delete(:__struct__) |> Map.keys() do
    defmultiop unquote(:"env_set_#{attrib}"), parens: 0, in: [exec: [a | exec_rest]] do
      env |> set_attrib(unquote(attrib), a) |> Environment.update_stack(:exec, exec_rest)
    end
  end

  # defmultiop set_evalpush_limit, in: [exec: [a | exec]] do
  #  %Environment{stacks: stacks} = env
  #  %Environment{evalpush_limit: a, stacks: %{stacks | exec: exec}}
  # end
end
