defmodule InliningTools do
  @moduledoc """
  `use InliningTools` and then add
  """
  @doc false
  defmacro __using__(_opts) do
    quote do
      import InliningTools
      @before_compile InliningTools
      Module.register_attribute(__MODULE__, :inlines, accumulate: true)
      @compile :inline_list_funcs
    end
  end

  defmacro __before_compile__(_env) do
    quote do
      @compile {:inline,
                for kw <- @inlines |> Enum.uniq() do
                  kw
                end}
    end
  end

  #  defmacro inline(def_call) do
  #    quote bind_quoted: [def_call: Macro.escape(def_call, unquote: true)] do
  #      {:def}
  #    end
  #  end
end
