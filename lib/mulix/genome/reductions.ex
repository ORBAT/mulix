defmodule Reductions do
  @moduledoc """
  Utility functions for reducing lists.
  """
  defmacro __using__(_opts) do
    quote do
      import Reductions
    end
  end

  use InliningTools

  # [0.1, 0.2, 0.3]
  # fun +

  # acc = [0.1], xs = [0.2, 0.3]

  # acc = [0.1], item = 0.2, xs = [0.3]
  # [fun(hd(acc), item) | acc]

  @compile :inline_list_funcs

  @doc ~S"""
    Returns a list of the intermediate values of the reduction of `list` with `fun`.

      iex> Reductions.reductions([1, 2, 3, 4], &+/2)
      [1, 3, 6, 10]
  """
  def reductions(list, fun)

  def reductions([x | xs], fun) do
    # :lists.reverse(List.foldl(xs, [x], fn item, acc -> [fun.(item, hd(acc)) | acc] end))
    :lists.reverse(:lists.foldl(fn item, acc -> [fun.(item, hd(acc)) | acc] end, [x], xs))
  end

  @inlines {:reductions, 2}

  @doc ~S"""
  Run `fun` on `item` `count` times.

      iex> Reductions.reduce_times(1, &(&1 + 1), 3)
      4
  """
  @spec reduce_times(item_type, (item_type -> item_type), non_neg_integer()) :: item_type
        when item_type: var
  def reduce_times(item, fun, count)
  def reduce_times(item, _fun, count) when count <= 0, do: item

  def reduce_times(item, fun, 1) do
    fun.(item)
  end

  def reduce_times(item, fun, count) do
    reduce_times(fun.(item), fun, count - 1)
  end

  @inlines {:reduce_times, 3}

  @doc ~S"""
  `index_of_probability` returns the index of a random element from `probabilities`.

  `probabilities` should sum to 1. Example: if an element at idx 4 of `probabilities` is `0.5`,
  `index_of_probability` will have a 50% chance of returning `4`

  """
  @spec index_of_probability([float()]) :: non_neg_integer()
  def index_of_probability(probabilities) do
    prob = :rand.uniform()
    index_of_probability_(reductions(probabilities, &+/2) ++ [1], prob, 0)
  end

  @inlines {:index_of_probability, 1}

  @spec index_of_probability_(list(float()), float(), non_neg_integer()) :: non_neg_integer()
  defp index_of_probability_([head_prob | _], prob, idx) when prob <= head_prob do
    idx
  end

  defp index_of_probability_([_ | rest_probs], prob, idx) do
    index_of_probability_(rest_probs, prob, idx + 1)
  end

  defp index_of_probability_([], _p, idx) do
    idx
  end
end
