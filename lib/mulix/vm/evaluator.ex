defmodule Evaluator.Guards do
  @moduledoc """
  Contains guard functions for expression types.
  """

  import Record

  defguard is_literal(expr)
           when expr != nil and
                  (is_number(expr) or is_boolean(expr) or is_bitstring(expr) or is_atom(expr))

  defguard is_expression(expr) when is_literal(expr) or is_list(expr)

  defguard is_op(expr) when is_record(expr)
end

defmodule Evaluator do
  @moduledoc """
  `Evaluator` specifies a behaviour for modules that can evaluate expressions.

  See `Evaluator.Mu` for the phenome evaluator, and `Genome.Mush` for the genome evaluator.
  """
  @type evaluate :: (Environment.t(), Type.expr() -> Environment.t())

  @callback evaluate(env :: Environment.t(), expr :: Type.expr()) :: Environment.t()
end

# TODO(ORBAT): how to do random code generation in `Mu`?

# A randomly generated expression should be deterministic somehow.

defmodule Evaluator.Mu do
  @moduledoc """
  `Evaluator.Mu` is the default phenome `Evaluator`, i.e. the evaluator that's used for
  calculations. This is in contrast to `Genome` evaluators that are used for producing phenomes.
  """
  @behaviour Evaluator

  import Op.Record
  import Evaluator.Guards

  import Environment

  @spec evaluate(Environment.t(), Type.expr()) :: Environment.t()
  @doc ~S"""
  Evaluate an expression against an environment, and return a possibly changed environment.

  Note that this doesn't e.g. decrement `EVALPUSH-LIMIT`, push `expr` to the `:code` stack before
  execution etc. You'll want `Environment.step/1` for anything more than just evaluating the
  expression.

  An expression is either an `Op`, a supported primitive type (number, boolean, bitstring, atom), or
  a list of expressions.
  """
  @impl true
  def evaluate(env, expr)

  def evaluate(env, oper(op: item)) do
    cond do
      is_function(item) ->
        env |> item.()

      is_expression(item) ->
        env |> evaluate(item)
    end
  end

  def evaluate(env, expr) when is_number(expr) do
    env |> push(:number, expr)
  end

  def evaluate(env, expr) when is_boolean(expr) do
    env |> push(:boolean, expr)
  end

  def evaluate(env, expr) when is_bitstring(expr) do
    env |> push(:string, expr)
  end

  # if expr is is a type that has a stack in this env, push it on the type stack.
  # otherwise
  def evaluate(env, expr) when is_atom(expr) do
    cond do
      type_defined?(env, expr) ->
        env |> push(:type, expr)

      stack_defined?(env, expr) ->
        env |> push(:stack, expr)

      true ->
        case get_name(env, expr) do
          nil -> env |> push(:name, expr)
          expr -> env |> evaluate(expr)
        end
    end

    #    if env |> type_defined?(expr) do
    #
    #    else
    #      case env |> name(expr) do
    #        nil -> env |> push(:name, expr)
    #        expr -> env |> evaluate(expr)
    #      end
    #    end
  end

  def evaluate(env, []), do: env

  def evaluate(env, expr) when is_list(expr) do
    env
    |> push(:exec, expr)
  end
end
