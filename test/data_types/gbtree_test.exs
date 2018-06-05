defmodule GBTreeTest do
  use ExUnit.Case, async: true
  doctest GBTree.Shim
  doctest GBTree

  #  def gen_test_item(n \\ 0), do: {String.to_atom(<<?a + n>>), n}
  #
  #  def gen_test_items(from, to), do: Enum.map(from..to, &gen_test_item/1)
  #
end
