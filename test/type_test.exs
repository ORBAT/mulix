defmodule TypeTest do
  use ExUnit.Case, async: true
  doctest Type
  doctest Type.Convert
  doctest Type.Guards

  setup do
    {:ok,
     exprs: [
       [1, 2, 3],
       Type.boolean(),
       :some_name,
       "a string",
       1234.5,
       Op.Number.div()
     ],
     types: [Type.code(), Type.type(), Type.name(), Type.string(), Type.number(), Type.code()]}
  end

  test "type_of", context do
    for {expr, type} <- List.zip([context[:exprs], context[:types]]) do
      assert Type.type_of(expr) == type
    end
  end

  describe "converting" do
    alias Type.Convert, as: Conv

    test "names to types adds prefix when it can" do
      assert Type.type() = :boolean |> Conv.to(Type.type()) |> Type.type_of()
    end

    test "types to names removes prefix" do
      assert Type.name() = Type.boolean() |> Conv.to(Type.name()) |> Type.type_of()
    end

    test "any type to another", context do
      for type <- Type.types(), expr <- context[:exprs] do
        case type do
          # code shouldn't change the expr at all, but output type will still be whatever the input
          # was
          Type.code() ->
            assert expr == expr |> Conv.to(type)
            assert Type.type_of(expr) == expr |> Conv.to(type) |> Type.type_of()

          _ ->
            assert Type.type_of(expr |> Conv.to(type)) == type
        end
      end
    end
  end
end
