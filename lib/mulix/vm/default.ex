defmodule Default do
  defmodule Types do
    import AttribTools

    @compile :inline_list_funcs

    @type stack_to_type :: %{Stack.name() => Type.t()}

    require Type

    @doc """
    The default phenome stack names and types
    """
    @spec phenome_stack_to_type() :: %{Stack.name() => Type.t()}
    attrib_and_getter(
      phenome_stack_to_type,
      %{
        exec: Type.code(),
        code: Type.code(),
        number: Type.number(),
        boolean: Type.boolean(),
        string: Type.string(),
        name: Type.name(),
        error: Type.code(),
        type: Type.type(),
        stack: Type.name(),
        return: Type.code(),
        aux: Type.code()
      }
    )

    @doc """
    The default genome stack names and types
    """
    attrib_and_getter(
      genome_stack_to_type,
      %{blocks: Type.code(), curr_block: Type.code()} |> Map.merge(@phenome_stack_to_type)
    )

    @doc """
    `to_stacks` takes a map of stack names to types, and returns a map of stack names to empty
    stacks
    """
    @spec to_stacks(stack_to_type()) :: %{Stack.name() => []}
    def to_stacks(stack_to_type_map),
      do: stack_to_type_map |> Enum.map(fn {stack, _} -> {stack, []} end) |> Enum.into(%{})

    @doc """
    `type_to_stacks_map` takes a map of stack names to types, and returns a map of types to stack
    names. The stack names are in alphabetical order
    """
    @spec type_to_stacks_map(stack_to_type()) :: %{Type.t() => [Stack.name()]}
    def type_to_stacks_map(stack_to_type_map),
      do:
        stack_to_type_map
        |> Enum.into([])
        |> Enum.group_by(&elem(&1, 1))
        |> Enum.map(fn {type, stack_kw} -> {type, Keyword.keys(stack_kw) |> Enum.sort()} end)
        |> Enum.into(%{})

    @doc """
    Type to default to if a type is needed but either the current type isn't compatible, or there is
    no current type
    """
    attrib_and_getter(default_type, Type.code())

    @doc """
    Stack to default to if a stack is needed but either the current stack isn't compatible, or there
    is no current stack
    """
    attrib_and_getter(default_stack, :code)
  end

  import AttribTools

  use Op.Record

  @compile :inline_list_funcs

  attrib_and_getter(op_modules, [Op.Number, Op.Exec, Op.Stacks, Op.Code, Op.Tag])

  @spec defined_names_map([module]) :: %{atom => Type.expr()}
  def defined_names_map(modules \\ @op_modules) do
    for(
      op = oper(name: name) <- List.flatten(Enum.map(modules, fn mod -> mod.ops() end)),
      into: %{}
    ) do
      {name, op}
    end
  end

  # TODO(ORBAT): use max_points

  @doc """
  The default maximum number of steps allowed per execution. One instruction or literal is one step,
  and descending into a list (so each `[]`) is one step.
  """
  @spec evalpush_limit() :: integer()
  attrib_and_getter(evalpush_limit, 1000)

  @doc """
  Default maximum number of points. Defaults to 100
  """
  attrib_and_getter(max_points, 100)

  @doc """
  Default bottom type. Defaults to `:code`
  """
  attrib_and_getter(input_bottom_type, :code)

  @doc """
  Maximum `GBTree` `delete` operations in a row before forcing a rebalance
  """
  attrib_and_getter(max_gbtree_deletes, 4)
end
