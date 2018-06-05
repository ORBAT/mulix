defmodule GBTree.Shim do
  alias :gb_trees, as: GB

  @opaque t :: GB.tree()
  @opaque t(key, value) :: GB.tree(key, value)

  @type item :: any

  @typedoc """
  Anything you can compare with the standard `<`, `>=` etc.
  """
  @type comparable :: any()

  @typedoc """
  All keys in the tree must be comparable, and should probably be of the same type.
  """
  @type key :: comparable

  @type kv() :: {key, item}

  @type kv(k, v) :: {k, v}

  @spec new() :: t
  @doc ~S"""
  Create new, empty `GBTree.Shim`.
  """
  def new(), do: GB.empty()

  @doc ~S"""
  Create new `GBTree.Shim` with `key` set to `value`
  """
  @spec new(k, v) :: t(k, v) when k: key, v: item
  def new(key, value), do: GB.from_orddict([{key, value}])

  @doc """
  Creates a new `GBTree.Shim` from list `list`. `list` should contain items of type `t:kv/0`, eg.
  `[a: 10, b: 20]` or `[{12, {:a, 10}}]`

  `list` does not have to be sorted.


      iex> GBTree.Shim.new_from_list([q: 1, a: 7, c: 666])
      ...> |> GBTree.Shim.get_closest_key(:b)
      {:c, 666}
  """
  @spec new_from_list([kv(k, v)]) :: t(k, v) when k: key, v: item
  def new_from_list(list), do: GB.from_orddict(Enum.sort(list))

  @spec enter(t, key, item) :: t
  def enter(tree, key, value), do: GB.enter(key, value, tree)

  @type acc :: any

  @type walker() :: (t, iter | :halt, acc -> {t, iter | :halt, acc})

  defp walk_closest_keys_(tree, count, iter, fun, acc)

  defp walk_closest_keys_(tree, _count, :halt, _fun, acc), do: {tree, acc}

  defp walk_closest_keys_(tree, 0, _iter, fun, acc) do
    {new_tree, _, new_acc} = fun.(tree, :halt, acc)
    {new_tree, new_acc}
  end

  defp walk_closest_keys_(tree, count, iter, fun, acc) do
    next_iter =
      case GB.next(iter) do
        :none ->
          {min_key, _} = GB.smallest(tree)
          {_key_after_min, _val, iter_after_min} = GB.next(iterator_from(tree, min_key))
          iter_after_min

        {_, _, next_iter} ->
          next_iter
      end

    {fun_tree, fun_iter, fun_acc} = tree |> fun.(tree, next_iter, acc)
    fun_tree |> walk_closest_keys(count - 1, fun_iter, fun, fun_acc)
  end

  @spec walk_closest_keys(t, non_neg_integer, key, walker(), acc) :: any
  def walk_closest_keys(tree, count, key, fun, acc) do
    max_count = min(size(tree), count)

    case max_count do
      0 ->
        nil

      _ ->
        iter = iterator_from(tree, key)
        {fun_tree, fun_iter, fun_acc} = tree |> fun.(tree, iter, acc)
        walk_closest_keys_(fun_tree, max_count - 1, fun_iter, fun, fun_acc)
    end
  end

  @spec get_closest_keys(t, non_neg_integer, iter, [kv]) :: [kv]
  defp get_closest_keys(tree, count, iter, acc)

  defp get_closest_keys(_tree, 0, _iter, acc), do: acc

  defp get_closest_keys(_tree, _count, :none, acc) do
    acc
  end

  defp get_closest_keys(tree, count, iter, acc) do
    {kv, next_iter} =
      case GB.next(iter) do
        :none ->
          kv = {min_key, _} = GB.smallest(tree)
          {_key_after_min, _val, iter2} = GB.next(iterator_from(tree, min_key))
          {kv, iter2}

        {key, item, iter2} ->
          {{key, item}, iter2}
      end

    tree |> get_closest_keys(count - 1, next_iter, [kv | acc])
  end

  @doc ~S"""
  Gets the `count` closest keys to `key`, wrapping around at the bottom if necessary. The closest
  key is the head of the returned list.

  Will return at most `GBTree.Shim.size(tree)` (`GBTree.Shim.size/1`) items
  """
  @spec get_closest_keys(t, key, non_neg_integer()) :: [kv]
  def get_closest_keys(tree, key, count) do
    max_count = min(size(tree), count)
    :lists.reverse(tree |> get_closest_keys(max_count, iterator_from(tree, key), []))
  end

  @doc ~S"""
  `get_closest_key` returns the first item that is greater than or equal to `key` (rolling around
  to the bottom if none is found). Returns `:none` if the `GBTree.Shim` is empty.

      # exact match
      iex> GBTree.Shim.new_from_list([q: 1, a: 7, c: 666])
      ...> |> GBTree.Shim.get_closest_key(:a)
      {:a, 7}

      # match above wanted key
      iex> GBTree.Shim.new_from_list([q: 1, a: 7, c: 666])
      ...> |> GBTree.Shim.get_closest_key(:d)
      {:q, 1}

      # no bigger keys
      iex> GBTree.Shim.new_from_list([q: 1, a: 7, c: 666])
      ...> |> GBTree.Shim.get_closest_key(:r)
      {:a, 7}


      # empty tree
      iex> GBTree.Shim.new()
      ...> |> GBTree.Shim.get_closest_key(:r)
      :none
  """
  @spec get_closest_key(t, key) :: kv | :none
  def get_closest_key(tree, key)
  def get_closest_key({0, _}, _key), do: :none

  def get_closest_key(tree, key) do
    case GB.next(GB.iterator_from(key, tree)) do
      :none ->
        GB.smallest(tree)

      {key, item, _iter2} ->
        {key, item}
    end
  end

  @spec take_exact(t, key) :: {kv | :none, t}
  def take_exact(tree, key) do
    case GB.take_any(key, tree) do
      :error -> {:none, tree}
      {value, tree2} -> {{key, value}, tree2}
    end
  end

  def take_nearest(tree, key) do
  end

  def delete_exact(tree, key) do
    :gb_trees.delete_any(key, tree)
  end

  def delete_nearest()

  defdelegate size(tree), to: GB

  def empty?({0, _}), do: true
  def empty?(_), do: false

  @spec balance(t) :: t
  defdelegate balance(tree), to: GB

  @opaque iter(key, item) :: :gb_trees.iter(key, item)
  @opaque iter() :: iter(any, any)

  @spec iterator(t) :: iter
  defdelegate iterator(tree), to: GB

  @spec iterator_next(iter(k, v)) :: :none | {k, v, iter(k, v)} when k: key, v: item
  defdelegate iterator_next(iter), to: GB, as: :next

  @spec iterator_from(t, k) :: iter(k, item) when k: key
  def iterator_from(tree, key), do: GB.iterator_from(key, tree)
end

defmodule GBTree do
  import Algae
  alias GBTree.Shim, as: TShim

  defdata do
    tree_data :: TShim.t() \\ TShim.new()
    num_deletes_in_row :: non_neg_integer()
  end

  def enter(tree, key, item)

  def enter(tree = %GBTree{tree_data: tree_data}, key, item) do
    %GBTree{tree | tree_data: tree_data |> TShim.enter(key, item), num_deletes_in_row: 0}
  end

  @spec get_closest(t, TShim.key()) :: TShim.kv() | :none
  def get_closest(tree, key)

  def get_closest(%GBTree{tree_data: tree_data}, key) do
    tree_data |> TShim.get_closest_key(key)
  end

  @doc ~S"""
  Deletes `key` from `tree` if there is an exact match, and rebalances the tree if there have been
  `Default.max_gbtree_deletes()` deletes in a row.

      iex> {kv, _tree} = GBTree.new(GBTree.Shim.new(10, :a))
      ...> |> GBTree.take_exact(10)
      ...> kv
      {10, :a}
  """
  @spec take_exact(t(), k) :: {TShim.kv(k, TShim.item()) | :none, t()}
        when k: TShim.key()
  def take_exact(tree = %GBTree{num_deletes_in_row: orig_dels, tree_data: tree_data}, key) do
    case tree_data |> TShim.take_exact(key) do
      nope = {:none, _} ->
        nope

      {kv, tree_data2} ->
        dels = orig_dels + 1

        cond do
          dels > Default.max_gbtree_deletes() ->
            {kv, %GBTree{tree | num_deletes_in_row: 0, tree_data: tree_data2 |> TShim.balance()}}

          true ->
            {kv, %GBTree{tree | num_deletes_in_row: dels, tree_data: tree_data2}}
        end
    end
  end
end
