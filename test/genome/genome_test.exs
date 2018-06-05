defmodule GenomeTest do
  use ExUnit.Case, async: true
  doctest Genome
  doctest Genome.Mush

  alias Genome.Mush

  describe "Mush.evaluate" do
    test "with program that doesn't close all parens, has blocks and curr_block" do
      # mush_env =
      # ...> Genome.new_env()
      # ...> |> Environment.update_stack(:blocks, [[]])
      # ...> |> Environment.update_stack(:curr_block, [[:number_drop], [:y, :sub, :mul]])
      # ...> |> Environment.update_stack(:code, [:if, 7, 1])
      # ...> |> Genome.Mush.close_parens(1)
      # ...> |> Environment.get_stacks([:code, :blocks, :curr_block])
      # %{
      #   blocks: [[]],
      #   code: [:if, 7, 1],
      #   curr_block: [[[:number_drop], :y, :sub, :mul]]
      # }
      assert [[], [[:drop], :y, :sub, :mul], :if, 7, 1] =
               Genome.new_env()
               |> Environment.update_stack(:blocks, [[]])
               |> Environment.update_stack(:curr_block, [[], [:y, :sub, :mul]])
               |> Environment.update_stack(:code, [:if, 7, 1])
               |> Mush.evaluate(Mush.genome(item: :drop, close: 0))
               |> Environment.get_stack(:code)
    end
  end

  describe "Mush handle_close" do
    test "with close = 0"
    test "with close > 0, open current block, nothing in blocks"
    test "with close > 0, open current block, something in blocks"
  end
end
