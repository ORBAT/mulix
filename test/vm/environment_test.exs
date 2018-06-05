defmodule EnvTest do
  use ExUnit.Case, async: true
  doctest Environment

  import Op.Record
  alias Evaluator.Mu

  test "push to exec" do
    assert [1, 2, 3] =
             Environment.new()
             |> Environment.push(:exec, [1, 2, 3])
             |> Environment.get_stack(:exec)
  end

  describe "execution" do
    import Op

    defmodule ExecContents do
      # test that when an operator is executed, it will be pushed to :code and popped from :exec
      # first
      def test_op_eval() do
        Op.new(
          fn %Environment{stacks: %{exec: exec, code: code}} ->
            assert [] = exec
            [:test_op_eval] = code
          end,
          :test_op_eval
        )
      end
    end

    test "should push whole program onto :code on first step" do
      assert [[1, 2, 3, 4]] =
               Environment.new()
               |> Environment.push(:exec, [[1, 2, 3, 4]])
               |> Environment.step()
               |> Environment.get_stack(:code)
    end

    @tag :exec_onto_code
    test "should push whole program onto :code when executing, plus each individual op" do
      assert [4, 3, 2, 1, [1, 2, 3, 4]] =
               Environment.new()
               |> Environment.execute([1, 2, 3, 4])
               |> Environment.get_stack(:code)
    end

    test "op step should decrement evalpush_limit" do
      use Environment.Attribs

      match_attrib(:evalpush_limit, el) =
        Environment.new()
        |> Environment.push(:exec, 1)
        |> Environment.step()

      assert Default.evalpush_limit() - 1 == el
    end

    test "should stop with an error if evalpush-limit is exceeded" do
      assert %Environment{stacks: %{error: [:evalpush_limit_exceeded]}} =
               Environment.new()
               |> Environment.execute([1, Op.Exec.y(), [1, Op.Number.add()]])
    end

    test "step with op as exec head should not have the op on exec stack, but on code" do
      Environment.new()
      |> Environment.add_name(ExecContents.test_op_eval())
      |> Environment.push(:exec, :test_op_eval)
      |> Environment.step()
    end

    test "step on list should 'expand' the list" do
      exec =
        Environment.new()
        |> Environment.push(:exec, [[4, 3, 2, 1, Op.Number.add()], 10])
        |> Environment.step()
        |> Environment.get_stack(:exec)

      # TODO(ORBAT): why does putting assert in front of this fail with "illegal pattern"?!
      [4, 3, 2, 1, oper(name: :add), 10] = exec
    end

    test "should run until :exec is empty" do
      nums =
        Environment.new()
        |> Environment.push(:number, [3, 2, 1])
        |> Environment.execute([Op.Number.add(), Op.Number.mul()])
        |> Environment.get_stack(:number)

      assert [9] = nums
    end
  end

  describe "attrib ops" do
    test "should set their respective attrib" do
      use Environment.Attribs

      env =
        match_attrib(:evalpush_limit, el) =
        Environment.new()
        |> Environment.add_name(Environment.AttrOps.env_set_evalpush_limit())
        |> Environment.execute([:env_set_evalpush_limit, -1])

      assert el == -1
      assert [] = env.stacks[:exec]
    end
  end
end
