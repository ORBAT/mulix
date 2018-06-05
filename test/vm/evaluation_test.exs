defmodule EvaluatorTest do
  use ExUnit.Case, async: true
  doctest Evaluator

  import Op.Record
  alias Evaluator.Mu

  describe "names" do
    test "that point to functions should call them" do
      assert %Environment{stacks: %{number: [-1]}} =
               Environment.new() |> Environment.push(:number, 1) |> Mu.evaluate(:neg)
    end

    test "that are not registered should end up on :name" do
      assert %Environment{stacks: %{name: [:bler]}} = Environment.new() |> Mu.evaluate(:bler)
    end
  end

  test "number" do
    assert %Environment{stacks: %{number: [1]}} = Environment.new() |> Mu.evaluate(1)
  end

  test "bool" do
    assert %Environment{stacks: %{boolean: [true]}} = Environment.new() |> Mu.evaluate(true)
  end

  test "string" do
    assert %Environment{stacks: %{string: ["bleb"]}} = Environment.new() |> Mu.evaluate("bleb")
  end

  test "op" do
    assert %Environment{stacks: %{number: [-1]}} =
             Environment.new()
             |> Environment.push(:number, 1)
             |> Mu.evaluate(Op.Number.neg())
  end
end
