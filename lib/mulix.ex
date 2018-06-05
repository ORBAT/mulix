defmodule Mulix do
  @moduledoc """
  µlix (Mulix) is a genetic programming toolkit for Elixir. It implements a vaguely [Push
  3.0](http://faculty.hampshire.edu/lspector/push3-description.html)-like virtual machine.

  ## Description of the execution process

  `Op`s define virtual machine instructions / operations, and they form _expressions_. An expression
  can be a literal (number, bool, binary, atom), an `Op`, or a list of expressions.

  An `Environment` struct contains stacks (`:exec`,`:code`,`:number`,`:boolean`,`:string`,`:name`
  (atoms), `:type` `:error`) that expressions can mutate, and configurations related to execution.
  Literals are pushed to their respective stacks. An `Op` takes in an `Environment`, and return a
  possibly mutated version.

  An `Individual` has both a _phenome_ (top-level expression + an `Environment` with a set of stacks
  and defined names suitable for executing expressions), and a _genome_ (an `Environment` created with
  `Genome`, with defined names that generate the phenome.)

  # IDEA: add a `:tag` type


  # IDEA: tag-based co-operation
  http://faculty.hampshire.edu/lspector/pubs/multitags-GPTP06.pdf
  https://pdfs.semanticscholar.org/f03f/ebca2a77b16fe8a04c4ae377e4bfdfa84ea9.pdf
  - each individual has a multidimensional tag
  - tag inherited from parent, + mutation

  https://erp12.github.io/push-redux/pages/names_and_tags/index.html
  https://www.lri.fr/~hansen/proceedings/2011/GECCO/proceedings/p1419.pdf

  # IDEA: a `World` with a coordinate system

  - world has `num_individuals`
  - each individual has a position in R^num_err_fn, where `num_err_fn` is the number of defined
    error functions `fit_fn_i` (at least output error). Each axis is capped at `worst_fitness` (e.g. 1000)
  - total fitness of an individual is distance from origin
  - fitness functions have a rank, so `[:fit_fn2, :fn0, :fn1]` would mean `:fit_fn2` has more weight than
    `:fit_fn0` etc.
  - world has `food_units_per_pos` "food" at each position `{0..worst_fitness, 0..worst_fitness,
    ...}`
  - individuals need `food_intake` (dependent on steps taken during execution & size) units of food
    within some radius `eat_radius`
  - individuals eat in fitness order. They are sorted by `total_fitness`, then divided into groups
    of size `ratio_in_food_group * num_individuals`. Then for each food group in total fitness
    order: for each fitness function `fit_fn_i` in rank order, take `food_tournament_size`
    individuals at random, sort by `fit_fn_i`, choose nth best with probability `food_tournament_p *
    (1-f_t_p^n)`
  - individuals can store at most `max_food_storage` units of food
  - if `food_stored` goes to 0, the individual dies

  # IDEA: autoconstructive evolution
  See https://www.lri.fr/~hansen/proceedings/2013/GECCO/companion/p1627.pdf

  """
end
