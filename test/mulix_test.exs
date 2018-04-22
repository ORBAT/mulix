defmodule MulixTest do
  use ExUnit.Case
  doctest Mulix

  test "greets the world" do
    assert Mulix.hello() == :world
  end
end
