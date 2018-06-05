# TODO: Implement Genome for plush-like (mush / µsh?) + binary
# TODO: make binary proto produce mush

# defprotocol Genome.Translator do
#   @moduledoc """
#   Anything implementing `Genome.Translator` can take an `Environment`
#   """
#
#   @spec to_mush_program(Environment.t(), any()) :: Environment.t()
#   def to_mush_program(env, representation)
#
#   @spec random_genome(Environment.t()) :: any()
#   def random_genome(env)
# end

defmodule Genome do
  # use Op

  @typedoc """
  A `t:repr/0` is a genome's representation. It may be anything: binary, lists, proper µ (`Mu`) code
  , a number etc.
  """
  @type repr() :: any()

  @doc """
  Turn some arbitrary data type to a µsh `Environment`.

  Must produce an initialized `Environment` with the `:blocks` and `:curr_block` stacks so it's
  suitable for running µsh.
  """
  @callback to_mush(representation :: repr()) :: Environment.t()
  @callback from_mush(env :: Environment.t()) :: repr()

  @doc ~S"""
  Take a "normal" expr `expr`, and turn it into a genome's instruction representation.
  """
  @callback from_expr(attrs :: Environment.Attribs.t(), expr :: Type.expr()) :: any()

  @dialyzer {[:no_contracts, :no_return], [new_env: 0, new_env: 1]}
  @spec new_env(module) :: Environment.t()
  def new_env(evaluator \\ Genome.Mush) do
    # TODO(ORBAT): figure out the env needed for generation
    Environment.new(evaluator, Default.Types.genome_stack_to_type())
  end
end

defmodule Genome.Mush do
  @moduledoc """
  `Genome.Mush` defines a [Plush](https://push-language.hampshire.edu/t/plush-genomes/279)-like
  genome `Evaluator` that takes a linear genome representation and turns it into a phenome.

  The `Environment` used by `Genome.Mush` has the extra stacks `:blocks` and `:curr_block`, and a set of
  defined names that operate on linear
  [Plush](https://push-language.hampshire.edu/t/plush-genomes/279)-like genomes.

  """

  @behaviour Evaluator
  @behaviour Genome

  require Record
  use InliningTools

  Record.defrecord(:genome, item: 0, silent: false, close: 0, parens: nil)

  @type t ::
          record(
            :genome,
            item: Type.expr(),
            silent: boolean(),
            close: non_neg_integer(),

            # if parens is nil, item's parens should be used
            parens: nil | non_neg_integer()
          )

  import Op.Record

  defmodule TranslateException do
    @moduledoc """
    Exception raised if there's a serious problem with translating a `Genome.Mush` genome to
    `Mu`
    """
    defexception message: "error translating µsh"
  end

  import Environment

  @close_paren_probabilities [0.772, 0.206, 0.021, 0.0008]

  @doc ~S"""
  `num_parens` returns an expression that calculates the number of parentheses a `t:Genome.Mush.t/0`
  operation should open. Genome `parens` overrides what the genome's item sets.

  If `parens` is non-nil in the genome itself, returns that number. If it is nil and the µsh op
  has `parens` set, that number is returned instead. If neither is set, returns 0.

  """
  @spec num_parens(Genome.Mush.t()) :: non_neg_integer()
  def num_parens(genome(parens: nil, item: oper(parens: parens))) do
    parens || 0
  end

  def num_parens(genome(parens: parens)) do
    parens || 0
  end

  @inlines {:num_parens, 1}

  use Reductions

  @doc """
  Generates a open/close parenthesis value for a µsh genome, using a list of probabilities that
  should sum to ~1. The first item of the list is the probability of generating a `close` of 0, the
  second for 1 and so on.

  Based on
  [Clojush](https://github.com/lspector/Clojush/blob/7bcf4a242e25c9205efcc0a8f9206b7bb34691b9/src/clojush/random.clj#L22-L40).
  """
  @spec gen_close_parens(list(float())) :: non_neg_integer()
  def gen_close_parens(probabilities \\ @close_paren_probabilities) do
    # prob = :rand.uniform()
    Reductions.index_of_probability(probabilities)
    # gen_close_parens_(reductions(probabilities, &+/2) ++ [1], prob, 0)
  end

  @inlines {:gen_close_parens, 1}

  def gen_extra_open_parens(expr, probabilities \\ @close_paren_probabilities) do
    case(Reductions.index_of_probability(probabilities)) do
      0 ->
        nil

      other ->
        sign_prob = :rand.uniform()

        sign =
          cond do
            sign_prob <= 0.5 -> -1
            true -> 1
          end

        Op.get_parens(expr) + sign * other
    end
  end

  @impl true
  def from_expr(attrs, expr) do
    # TODO: silence_probability from attrs
    silence_p = :rand.uniform()

    silence =
      cond do
        silence_p <= attrs.silence_probability -> true
        true -> false
      end

    genome(
      item: expr,
      close: gen_close_parens(),
      parens: gen_extra_open_parens(expr),
      silent: silence
    )
  end

  @inlines {:from_expr, 2}

  @gle """
  ```
  [{1, close:0}, {7, close:2}, {:if, close:0, parens:2},
    {:mul, close:0}, {:sub, close:0}, {:y, close:0, parens: 1},
        {:number_drop, close:2}
    {:neg, close:1},
    {:bleb, close:1}
    {99, close: 0}
  ]
  """

  # use Op
  # import Evaluator.Guards

  # @spec expr_to_mush(Environment.t(), Type.expr()) :: Op.t()

  # def number_of_consecutive_lists([]), do: 0
  # def number_of_consecutive_lists(expr) when is_literal(expr) when is_op(expr), do: 0

  # def number_of_consecutive_lists([expr | rest]) when is_list(expr) do
  #   1 + number_of_consecutive_lists(rest)
  # end

  # def expr_to_mush(env, [expr = oper(parens: 0) | list]) when is_list(list) do
  # end

  # def expr_to_mush(env, [expr | list]) when is_list(list) do
  # end

  # def expr_to_mush(env, expr) do
  #   fun = fn env -> env end
  #   Op.new(fun, :"mush_#{}")
  # end

  @doc ~S"""
    `close_parens` closes `closes` open blocks in `env`. If there are no open blocks,
    `close_parens` is a no-op.

      # close 1 open block when there are 2 open blocks, and 1 "pending" block
      iex> mush_env =
      ...> Genome.new_env()
      ...> |> Environment.update_stack(:blocks, [[]])
      ...> |> Environment.update_stack(:curr_block, [[:number_drop], [:y, :sub, :mul]])
      ...> |> Environment.update_stack(:code, [:if, 7, 1])
      ...> |> Genome.Mush.close_parens(1)
      ...> |> Environment.get_stacks([:code, :blocks, :curr_block])
      %{
        blocks: [[]],
        code: [:if, 7, 1],
        curr_block: [[[:number_drop], :y, :sub, :mul]]
      }

      # close 2 open blocks when there are 2 open blocks, and 1 "pending" block
      iex> mush_env =
      ...> Genome.new_env()
      ...> |> Environment.update_stack(:blocks, [[]])
      ...> |> Environment.update_stack(:curr_block, [[:number_drop], [:y, :sub, :mul]])
      ...> |> Environment.update_stack(:code, [:if, 7, 1])
      ...> |> Genome.Mush.close_parens(2)
      ...> |> Environment.get_stacks([:code, :blocks, :curr_block])
      %{
        blocks: [],
        code: [[[:number_drop], :y, :sub, :mul], :if, 7, 1],
        curr_block: [[]]
      }

      # close 3 open blocks when there are 2 open blocks, and 1 "pending" block
      iex> mush_env =
      ...> Genome.new_env()
      ...> |> Environment.update_stack(:blocks, [[]])
      ...> |> Environment.update_stack(:curr_block, [[:number_drop], [:y, :sub, :mul]])
      ...> |> Environment.update_stack(:code, [:if, 7, 1])
      ...> |> Genome.Mush.close_parens(3)
      ...> |> Environment.get_stacks([:code, :blocks, :curr_block])
      %{
        blocks: [],
        code: [[], [[:number_drop], :y, :sub, :mul], :if, 7, 1],
        curr_block: []
      }
  """
  def close_parens(env, closes)

  def close_parens(env, 0), do: env

  def close_parens(env = %Environment{stacks: %{blocks: [], curr_block: []}}, _closes) do
    env
  end

  def close_parens(
        env = %Environment{stacks: %{blocks: [[] | blocks_rest], curr_block: []}},
        closes
      )
      when is_integer(closes) and closes >= 0 do
    env
    |> push(:curr_block, [[]])
    |> update_stack(:blocks, blocks_rest)
    |> close_parens(closes - 1)
  end

  def close_parens(
        env = %Environment{
          stacks: %{curr_block: [top_block | curr_block_rest]}
        },
        closes
      )
      when is_integer(closes) and closes >= 0 and is_list(top_block) do
    curr_block_popped_env = env |> Environment.update_stack(:curr_block, curr_block_rest)

    # if curr_block is empty after pop, push popped block straight to :code.
    #
    # if it's not empty, then take the popped block and append it to the block on top of curr_block
    curr_block_handled_env =
      case curr_block_rest do
        [] ->
          curr_block_popped_env |> push(:code, [top_block])

        [sub_stack | xs] ->
          curr_block_popped_env
          |> Environment.update_stack(:curr_block, [[top_block | sub_stack] | xs])
      end

    # if after handling curr_block the blocks stack is not empty but curr_block is, pop a block off
    # blocks and push it to curr_block.
    #
    # otherwise do nothing.
    #
    # then take the env produced by the case and handle remaining closes
    case curr_block_handled_env do
      %Environment{stacks: stacks = %{blocks: [block | blocks_rest], curr_block: []}} ->
        %Environment{
          curr_block_handled_env
          | stacks: %{stacks | blocks: blocks_rest, curr_block: [block]}
        }

      _ ->
        curr_block_handled_env
    end
    |> close_parens(closes - 1)
  end

  # catch-all for weird situations
  def close_parens(%Environment{stacks: stacks}, closes) do
    raise TranslateException,
      message:
        "close_parens failed: curr_block or blocks in a weird state." <>
          " closes: #{inspect(closes, width: 0)}, " <>
          " curr_block: #{inspect(stacks.curr_block, width: 0)}, " <>
          "blocks: #{inspect(stacks.blocks, width: 0)}"
  end

  @doc ~S"""
  `open_blocks` opens `parens` new blocks. Opening 1 will have the new block on the `:curr_block`
  stack, and all > 1 will be pushed to `:blocks`.
  """
  @spec open_blocks(Environment.t(), non_neg_integer()) :: Environment.t()
  def open_blocks(env, parens)

  def open_blocks(env, 0), do: env

  def open_blocks(env, 1) do
    env |> push(:curr_block, [[]])
  end

  def open_blocks(env, parens) when is_integer(parens) do
    env |> push(:blocks, [[]]) |> open_blocks(parens - 1)
  end

  defp do_eval(
         env = %Environment{stacks: %{curr_block: curr_block}},
         gen_op = genome(item: item, close: close)
       ) do
    env_item_pushed =
      case curr_block do
        # no block open, so push straight to :code
        [] ->
          env
          |> push(:code, item)

        #         [sub_stack | xs] ->
        # curr_block_popped_env
        # |> Environment.update_stack(:curr_block, [[top_block | sub_stack] | xs])

        # a block is open so append to it
        [sub_stack | xs] ->
          env
          |> Environment.update_stack(
            :curr_block,
            [[item | sub_stack] | xs]
          )
      end

    env_item_pushed
    |> open_blocks(num_parens(gen_op))
    |> close_parens(close)
  end

  @doc ~S"""


  ```
  [1, 7, :if, [:mul, :sub, :y, [:number_drop]], [:neg], :bleb, 99]


  {1} -> code: [1], nothing in blocks/curr_block so push to code, blocks: [], curr_block : []

  {7} -> code [7, 1] ditto, blocks: [], curr_block: []
    close > 0 -> blocks & curr_block empty, do nothing

  {:if, parens: 2} -> code [:if, 7, 1], needs 2 blocks, 1st goes to curr_block, 2nd to blocks,  blocks: [[]], curr_block: [[]]

  {:mul} -> code -"-, curr_block not empty so push_to_top there, blocks: [[]], curr_block: [[:mul]]

  {:sub} -> code -"-, curr_block not empty so push_to_top curr_block: [[]] curr_block: [[:sub, :mul]]

  {y, parens: 1} -> code -"-, push_to_top :y, then only 1 block so *push* (not to top) [] to curr_block, blocks: [[]] curr_block:  [[],[y, :sub, :mul]]

  {:number_drop, close: 2} -> code -"-, curr_block not empty so push_to_top, blocks: [[]] curr_block: [[:number_drop],[y, :sub, :mul]]
    close > 0 -> if curr_block not empty, pop curr_block; then if curr_block not empty: push_to_top popped val to curr_block.
        curr_block: [[[:number_drop], :y, :sub, :mul]]
    if curr_block not empty, pop curr_block; then if curr_block empty: push to :code
        code: [[:number_drop], :y, :sub, :mul], :if, 7, 1], blocks: [[]], curr_block: []
    because curr_block is empty and blocks is not, pop blocks to curr_block
        blocks: [], curr_block: [[]]

  {:neg, close: 1} -> curr_block not empty so push_to_top, blocks: [], curr_block: [[:neg]]
    close > 0 -> if curr_block not empty, pop curr_block; then if curr_block empty: push to :code.
        code: [[:neg], [:number_drop], :y, :sub, :mul], :if, 7, 1], blocks: [], curr_block: []
  ...
  ```
  """
  @impl true
  @spec evaluate(Environment.t(), Type.expr()) :: Environment.t()
  def evaluate(env, expr)

  def evaluate(env, genome(silent: true)) do
    env
  end

  def evaluate(
        env = %Environment{stacks: %{exec: []}},
        gen_op
      ) do
    %Environment{stacks: new_stacks} = new_env = env |> do_eval(gen_op)

    case new_stacks.exec do
      [] ->
        need_to_close = length(new_stacks.blocks) + length(new_stacks.curr_block)
        new_env |> close_parens(need_to_close)

      _ ->
        new_env
    end
  end

  def evaluate(
        # = %Environment{stacks: %{blocks: blocks, curr_block: curr_block}},
        env,
        # = genome(item: item, close: close)
        gen_op
      ) do
    do_eval(env, gen_op)
    # dest_stack =
    #   case curr_block do
    #     # no block open, so push straight to :code
    #     [] ->
    #       :code

    #     # a block is open so append to it
    #     [list | _] when is_list(list) ->
    #       :curr_block
    #   end

    # env
    # |> push(dest_stack, item)
    # |> open_blocks(num_parens(gen_op))
    # |> close_parens(close)
  end
end
