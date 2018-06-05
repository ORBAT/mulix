defmodule StackTest do
  use ExUnit.Case, async: true
  doctest Stack

  test "pop from empty" do
    assert [] |> Stack.pop() == {:none, []}
  end
end
