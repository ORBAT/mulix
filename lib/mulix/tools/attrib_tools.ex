defmodule AttribTools do
  def attrib_ast(name, value, ctx),
    do: {:@, [context: Elixir, import: Kernel], [{name, [context: ctx], [value]}]}

  @doc """
  Takes a variable (so AST like {:atom, [...], [...]}) or an atom plus and a value, and turns them into
  `@name value`
  """
  defmacro attrib({name, _, _}, value) do
    # IO.inspect([name: name, value: value], label: "attrib, name")
    attrib_ast(name, value, __CALLER__.module)
  end

  defmacro attrib(name, value) when is_atom(name) do
    # IO.inspect([name: name, value: value], label: "attrib, atom")
    attrib_ast(name, value, __CALLER__.module)
  end

  def read_attrib_ast(atom, ctx) do
    {:@, [context: Elixir, import: Kernel], [Macro.var(atom, ctx)]}
  end

  defmacro read_attrib(name) do
    {:@, [], [name]}
  end

  defmacro attrib_and_getter(name = {atom, _, _}, value) do
    # IO.inspect([name: name, value: value], label: "attrib_and_getter")

    quote do
      Module.register_attribute(__MODULE__, unquote(atom), [])
      AttribTools.attrib(unquote(name), unquote(value))

      def unquote(atom)(), do: @unquote(name)
    end
  end

  defmacro attrib_and_getter(atom, value) when is_atom(atom) do
    # IO.inspect([atom: atom, value: value], label: "attrib_and_getter")

    quote do
      Module.register_attribute(__MODULE__, unquote(atom), [])
      AttribTools.attrib(unquote(atom), unquote(value))

      def unquote(atom)(), do: @unquote(Macro.var(atom, __CALLER__.module))
    end
  end
end
