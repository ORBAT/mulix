defmodule Datagen.Simple do
  def gen_kv_item(nth_from_a \\ 0), do: {String.to_atom(<<?a + nth_from_a>>), nth_from_a}

  @spec gen_kv_list(non_neg_integer, non_neg_integer) :: keyword(non_neg_integer)
  def gen_kv_list(from, to), do: Enum.map(from..to, &gen_kv_item/1)
end
