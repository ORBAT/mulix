defmodule Stack do
  @moduledoc ~S"""
  A general stack data type.

  ## Examples

  ```
  iex> Stack.new([3, 2, 1]) |> Stack.push(4)
  %Stack{stack: [4, 3, 2, 1]}

  iex> Stack.new([3, 2, 1]) |> Stack.pop()
  %Algae.Either.Right{right: {3, %Stack{stack: [2, 1]}}}

  iex> Stack.new([]) |> Stack.pop()
  %Algae.Either.Left{left: %Stack{stack: []}}
  ```
  """

  @typedoc """
  Type definition for non-empty stacks.
  """
  @type nonempty_stack() :: %Stack{stack: nonempty_list()}

  @typedoc """
  Type definition for empty stacks
  """
  @type empty_stack() :: %Stack{stack: []}

  require Algae
  import Algae
  alias Algae.Either.{Left, Right}

  defdata(list())

  @doc """
  Creates a new Stack from a list
  """
  def new(list), do: %Stack{stack: list}

  @spec push(Stack.t(), any) :: Stack.t()
  def push(%{stack: st} = stack, item) do
    %{stack | stack: [item | st]}
  end

  @doc """
  Pops an item off a stack. If the stack is not empty, Algae.Either.Right with the item and the stack will be returned
  """
  @spec pop(nonempty_stack()) :: %Algae.Either.Right{right: {any, Stack.t()}}
  def pop(%{stack: [item | rest]} = stack) do
    Right.new({item, %{stack | stack: rest}})
  end

  @spec pop(empty_stack()) :: %Algae.Either.Left{left: empty_stack()}
  def pop(%{stack: []} = stack) do
    Left.new(stack)
  end
end
