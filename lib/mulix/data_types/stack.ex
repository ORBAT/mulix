defmodule Stack do
  @moduledoc ~S"""
  A general stack data type.

  ## Examples

  ```
  iex> [3, 2, 1] |> Stack.push(4)
  [4, 3, 2, 1]

  iex> [3, 2, 1] |> Stack.pop()
  {3, [2, 1]}

  iex> [] |> Stack.pop()
  {:none, []}
  ```
  """

  @typedoc ~S"""
  In an `Environment`, each stack has an associated name
  """
  @type name() :: atom()

  @type t() :: list(any)
  @type t(item) :: list(item)

  @compile :inline_list_funcs

  @spec push(Stack.t(item), item) :: Stack.t(item) when item: var
  def push(stack, item) do
    [item | stack]
  end

  @type just(item) :: {item, [item]}
  @type nothing() :: {:none, []}

  @doc """
  Pops an item off a stack. If the list is not empty, returns `{item, rest}`, or `{:none, []}` if empty
  """
  @spec pop(Stack.t(item)) :: just(item) | nothing() when item: var
  def pop([]) do
    {:none, []}
  end

  def pop(lst) do
    {hd(lst), tl(lst)}
  end

  def drop([]), do: []
  def drop(list), do: tl(list)

  @doc ~S"""
  `( a b -- b )`

      iex> [3, 2, 1] |> Stack.nip()
      [3, 1]

      iex> [2, 1] |> Stack.nip()
      [2]

      iex> [3] |> Stack.nip()
      [3]
  """
  def nip([a, _ | rest]) do
    [a | rest]
  end

  def nip(other) do
    other
  end

  def dup([x | xs]), do: [x, x | xs]
  def dup(other), do: other

  def swap([a, b | rest]), do: [b, a | rest]
  def swap(other), do: other

  @doc ~S"""
  `( a b -- b a b)`
  """
  def over([a, b | rest]), do: [a, b, a | rest]

  def rot([c, b, a | rest]) do
    [a, c, b | rest]
  end

  def rot(other), do: other

  def push_to_top([sub_stack | xs], item) when is_list(sub_stack) do
    [[item | sub_stack] | xs]
  end

  # def push_to_top([], item), do: [[item]]

  def peek([]), do: :none
  def peek([x | _]), do: x
end
