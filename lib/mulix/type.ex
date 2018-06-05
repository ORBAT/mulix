defmodule Type.Tools do
  @moduledoc false
  def strip_type_prefix(atom) do
    case Atom.to_charlist(atom) do
      [?t, ?y, ?p, ?e, ?_ | type_name] -> :"#{type_name}"
      _ -> atom
    end
  end
end

defmodule Type do
  import Record, only: [is_record: 1]
  import AttribTools
  use InliningTools

  @type t :: :type_number | :type_boolean | :type_string | :type_name | :type_code | :type_type
  @type expr :: boolean | number | atom | binary | tuple | list(expr)

  @doc """
  List of supported types
  """
  attrib_and_getter(types, [
    :type_boolean,
    :type_code,
    :type_name,
    :type_number,
    :type_string,
    :type_type
  ])

  @inlines {:type?, 1}
  @spec type?(atom) :: boolean
  def type?(name)

  for t <- @types do
    def type?(unquote(t)), do: true

    type_name = Type.Tools.strip_type_prefix(t)

    @doc """
    Returns an atom for type `#{type_name}`
    """
    defmacro unquote(Macro.var(type_name, nil)) do
      unquote(t)
    end
  end

  def type?(_), do: false

  @inlines {:type_of, 1}
  @spec type_of(expr()) :: t()
  def type_of(expr) when is_list(expr), do: :type_code
  # records are always code
  def type_of(expr) when is_record(expr), do: :type_code
  def type_of(expr) when is_number(expr), do: :type_number
  def type_of(expr) when is_boolean(expr), do: :type_boolean
  def type_of(expr) when is_binary(expr), do: :type_string

  for type <- @types do
    def type_of(expr) when expr == unquote(type), do: :type_type
  end

  # if it's not a type, then it's a name
  def type_of(expr) when is_atom(expr), do: :type_name
end

defmodule Type.Guards do
  require Type
  import Record, only: [is_record: 1]

  types = Type.types()

  # this list is used to build is_type_type so that each type name matches using a guard
  is_type_compares =
    List.foldl(types, [], fn
      type_name, [] ->
        quote do
          var!(expr) === unquote(type_name)
        end

      type_name, acc ->
        quote do
          unquote(acc) or var!(expr) === unquote(type_name)
        end
    end)

  defguard is_type_type(expr) when unquote(is_type_compares)

  defguard is_type_code(expr) when is_list(expr) or is_record(expr)
  defguard is_type_string(expr) when is_binary(expr)
  defguard is_type_number(expr) when is_number(expr)
  defguard is_type_boolean(expr) when is_boolean(expr)
  defguard is_type_name(expr) when is_atom(expr)

  defguard is_type_match(expr, type)
           when type === Type.code() or (type === Type.type() and is_type_type(expr)) or
                  (type === Type.string() and is_type_string(expr)) or
                  (type === Type.number() and is_type_number(expr)) or
                  (type === Type.boolean() and is_type_boolean(expr)) or
                  (type === Type.name() and is_type_name(expr))

  defguard is_same_type(expr1, expr2)
           when (is_type_code(expr1) and is_type_code(expr2)) or
                  (is_type_string(expr1) and is_type_string(expr2)) or
                  (is_type_number(expr1) and is_type_number(expr2)) or
                  (is_type_boolean(expr1) and is_type_boolean(expr2)) or
                  (is_type_name(expr1) and is_type_name(expr2))
end

defmodule Type.Convert do
  use InliningTools
  require Type
  import Type.Guards
  import Record, only: [is_record: 1]

  defp safe_num_parse(str) when is_binary(str) do
    fun =
      if String.contains?(str, [<<?.>>, <<?E>>]) do
        &Float.parse/1
      else
        &Integer.parse/1
      end

    case fun.(String.trim(str)) do
      :error -> 0
      {num, _} -> num
    end
  end

  defp safe_num_parse(atom) when is_atom(atom), do: Atom.to_string(atom) |> safe_num_parse()

  defp err(from, to, this), do: {:error, "no way to convert '#{this}' (#{from}) to #{to}"}

  @inlines {:convert, 3}
  @spec convert(Type.t(), Type.t(), Type.expr()) :: {:ok, Type.expr()} | {:error, any()}
  def convert(same_type, same_type, this), do: this

  def convert(_, :code, this), do: {:ok, this}

  def convert(:name, :type, this) do
    cond do
      Type.type?(this) ->
        {:ok, this}

      Type.type?(with_prefix = :"type_#{this}") ->
        {:ok, with_prefix}

      true ->
        err(:name, :type, this)
    end
  end

  def convert(_, :name, this) when is_atom(this), do: {:ok, this}

  def convert(_, :name, this)
      when is_binary(this)
      when is_number(this)
      when is_boolean(this),
      do: :"#{this}"

  def convert(:code, :name, this) when is_tuple(this) do
    case elem(this, 0) do
      atom when is_atom(atom) -> {:ok, atom}
      _ -> err(:code, :name, this)
    end
  end

  # catch-all error
  def convert(from, to, this), do: err(from, to, this)

  # def add_items(a_type, b_type, item, prev)

  # def add_items(Type.number(), Type.number(), a, b), do: a + b

  # def add_items(Type.number(), Type.string(), a, b), do: <<inspect(a)::bitstring, b::bitstring>>
  # def add_items(Type.string(), Type.number(), a, b), do: b + safe_num_parse(a)

  # def add_items(Type.string(), Type.string(), a, b), do: <<a::bitstring, b::bitstring>>

  # def add_items(Type.boolean(), Type.boolean(), a, b), do: xor(a, b)
  # def add_items(Type.number(), Type.boolean(), a, b), do: xor(a != 0, b)
  # def add_items(Type.boolean(), Type.number(), a, b), do: bool_to_num(a) + b

  # [
  #   :type_boolean,
  #   :type_code,
  #   :type_name,
  #   :type_number,
  #   :type_string,
  #   :type_type
  # ]

  @doc """
  Convert `expr` to type `type`. Will always succeed.
  """

  @spec to(Type.expr(), Type.t()) :: Type.expr()
  def to(expr, type)

  def to(expr, Type.type()) when is_type_name(expr) do
    cond do
      Type.type?(expr) ->
        expr

      Type.type?(with_prefix = :"type_#{expr}") ->
        with_prefix

      true ->
        Default.Types.default_type()
    end
  end

  def to(expr, Type.name()) when is_type_type(expr), do: Type.Tools.strip_type_prefix(expr)

  def to(expr, type) when is_type_match(expr, type), do: expr

  def to("", Type.boolean()), do: false
  def to(expr, Type.boolean()) when is_type_string(expr), do: true

  def to(0, Type.boolean()), do: false
  def to(expr, Type.boolean()) when is_type_number(expr), do: true

  def to([], Type.boolean()), do: false
  def to(expr, Type.boolean()) when is_list(expr), do: true

  def to(_expr, Type.boolean()), do: false

  def to(expr, Type.name()) when is_list(expr), do: :none
  def to(expr, Type.name()) when is_record(expr), do: elem(expr, 0)
  def to(expr, Type.name()), do: Type.Tools.strip_type_prefix(:"#{expr}")

  def to(true, Type.number()), do: -1
  def to(false, Type.number()), do: 0
  def to(expr, Type.number()) when is_type_string(expr), do: safe_num_parse(expr)
  def to(expr, Type.number()) when is_atom(expr), do: Atom.to_string(expr) |> to(Type.number())

  def to(_expr, Type.number()), do: 0

  def to(expr, Type.string()) when is_atom(expr), do: Atom.to_string(expr)
  def to(expr, Type.string()) when is_record(expr), do: Atom.to_string(elem(expr, 0))
  def to(expr, Type.string()) when is_type_number(expr) or is_type_boolean(expr), do: "#{expr}"

  def to(_expr, Type.string()), do: ""

  # any atom that's a valid type is already caught by is_type_match
  def to(_expr, Type.type()), do: Default.Types.default_type()

  @inlines {:to, 2}
end
