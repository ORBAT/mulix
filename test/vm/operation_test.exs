defmodule OpTest do
  use ExUnit.Case, async: true
  doctest Stack
  import Op.Record
  alias Evaluator.Mu

  use Op

  setup do
    env =
      Environment.new()
      |> Environment.push(:exec, [Op.Number.sub(), Op.Number.mul()])
      |> Environment.push(:exec, [Op.Number.add()])
      # add, sub, mul
      |> Environment.push(:number, [1, 2, 3, 4, 5])
      |> Environment.push(:boolean, [true, false, true])
      |> Environment.push(:string, ["zot", "bleb", "qux"])

    {:ok, env: env}
  end

  describe "stack ops" do
    test "len", context do
      env =
        context[:env]
        |> Environment.push(:stack, :number)
        |> get_op(Op.Stacks.len()).()

      assert [5 | _] = env.stacks.number
    end

    test "other stack ops"
  end

  describe "defop" do
    test "creates a function that returns an op" do
      oper(name: :add) = Op.Number.add()
    end

    test "op only changes its own stack", context do
      env = context[:env] |> get_op(Op.Number.add()).()
      assert [9, 3, 2, 1] = env |> Environment.get_stack(:number)
      assert ["qux", "bleb", "zot"] = env |> Environment.get_stack(:string)
    end
  end

  describe "multi_op" do
    test "should preserve stacks it didn't touch", context do
      env = context[:env] |> get_op(Op.Exec.if()).()
      assert ["qux", "bleb", "zot"] = env |> Environment.get_stack(:string)
      assert [5, 4, 3, 2, 1] = env |> Environment.get_stack(:number)
    end

    test "if operation, true", context do
      env = context[:env] |> get_op(Op.Exec.if()).()
      # exec: ( mul sub add -- mul add)
      # boolean: ( true false true -- true false )
      exec = env |> Environment.get_stack(:exec)

      [oper(name: :add), oper(name: :mul)] = exec

      bool = env |> Environment.get_stack(:boolean)
      assert [false, true] = bool
    end

    test "if operation, false", context do
      env =
        context[:env]
        |> Environment.env_stack_op(:boolean, &Stack.drop/1)
        |> get_op(Op.Exec.if()).()

      # exec: ( mul sub add -- mul sub)
      # boolean: ( true false -- true )

      exec = env |> Environment.get_stack(:exec)
      [oper(name: :sub), oper(name: :mul)] = exec

      bool = env |> Environment.get_stack(:boolean)
      assert [true] = bool
    end
  end

  describe "defgenericop" do
    defmodule GenericTest do
      use Op

      require Type

      defgenericop :has_stack_var, _stack, default_inp_stack: :boolean do
        :boolean = input_stack
      end

      defgenericop :no_restrictions, stack, default_inp_stack: :number do
        [input_stack | stack]
      end

      defgenericop :restrictions, stack,
        input_stack: [:number, :code, :boolean, :aux],
        default_inp_stack: :number,
        binds: [:input_stack] do
        [input_stack | stack]
      end
    end

    test "should have access to the 'input_stack' variable in the body", context do
      context[:env] |> Mu.evaluate(GenericTest.has_stack_var())
    end

    test "should use default inp stack if stack stack is empty", context do
      env = context[:env] |> Mu.evaluate(GenericTest.no_restrictions())
      assert [:number | _] = env.stacks.number
    end

    test "should use current stack if no restrictions", context do
      env =
        context[:env]
        |> Environment.push(:stack, :code)
        |> Mu.evaluate(GenericTest.no_restrictions())

      assert [:code | _] = env.stacks.code
    end

    test "should use correct input stack when current stack is supported", context do
      for stack <- [:number, :code, :boolean, :aux] do
        env =
          context[:env]
          |> Environment.push(:stack, stack)
          |> Mu.evaluate(GenericTest.restrictions())

        assert [^stack | _] = env |> Environment.get_stack(stack)
      end
    end

    test "should use default input stack if current stack isn't supported", context do
      for stack <- [:string, :exec, :return, :stack] do
        env =
          context[:env]
          |> Environment.push(:stack, stack)
          |> Mu.evaluate(GenericTest.restrictions())

        assert [:number | _] = env.stacks.number
      end
    end
  end

  describe "defgenericop output" do
    defmodule OutputGenericTest do
      use Op

      defgenericop :one_output, _stack,
        output_stack: :exec,
        default_inp_stack: :number do
        input_stack
      end

      defgenericop :two_outputs, _stack,
        output_stack: [:exec, :aux],
        default_inp_stack: :number do
        input_stack
      end
    end

    test "to one stack"

    test "to multiple stacks, match found", context do
      for stack <- [:exec, :aux] do
        env =
          context[:env]
          |> Environment.push(:stack, [stack, :boolean])
          |> get_op(OutputGenericTest.two_outputs()).()

        assert [:boolean | _] = env.stacks[stack]
      end
    end
  end
end
