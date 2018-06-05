defmodule CA do
  def to_binary(n), do: Integer.digits(n, 2) |> left_pad(0)

  def left_pad(list, value) when length(list) < 8 do
    left_pad([value | list], value)
  end

  def left_pad(list, _, _), do: list

  defmacro rule_for(n) when is_integer(n) and n < 255 do
  end

  ## extract bits between a and b, a<=b
  #  unsigned createMask(unsigned a, unsigned b)
  # {
  #   unsigned r = 0;
  #   for (unsigned i=a; i<=b; i++)
  #       r |= 1 << i;
  #
  #   return r;
  # }

  #  def rule(n) do
  #    digits = to_binary(n)
  #
  #    rules =
  #      Enum.to_list(7..0)
  #      |> Enum.map(&to_binary(&1, 3))
  #
  #    map =
  #      Enum.zip(rules, digits)
  #      |> Map.new()
  #
  #    fn ns -> map[ns] end
  #  end
end

defmodule Bits do
  # this is the public api which allows you to pass any binary representation
  def extract(str) when is_bitstring(str) do
    extract(str, [])
  end

  # this function does the heavy lifting by matching the input binary to
  # a single bit and sends the rest of the bits recursively back to itself
  defp extract(<<b::size(1), bits::bitstring>>, acc) when is_bitstring(bits) do
    extract(bits, [b | acc])
  end

  # this is the terminal condition when we don't have anything more to extract
  defp extract(<<>>, acc), do: acc |> Enum.reverse()
end

# {:module, CA,
#  <<70, 79, 82, 49, 0, 0, 8, 100, 66, 69, 65, 77, 65, 116, 85, 56, 0, 0, 1, 8, 0,
#    0, 0, 29, 9, 69, 108, 105, 120, 105, 114, 46, 67, 65, 8, 95, 95, 105, 110,
#    102, 111, 95, 95, 9, 102, 117, 110, ...>>, {:left_pad, 3}}
# iex(38)> CA.rule(110)
# #Function<1.55408179/1 in CA.rule/1>
# iex(39)> CA.to_binary(110, 8)
# [0, 1, 1, 0, 1, 1, 1, 0]
# iex(40)> Enum.to_list(7..0) |> Enum.map(&(CA.to_binary(&1, 3)))
# [
#   [1, 1, 1],
#   [1, 1, 0],
#   [1, 0, 1],
#   [1, 0, 0],
#   [0, 1, 1],
#   [0, 1, 0],
#   [0, 0, 1],
#   [0, 0, 0]
# ]

# lst=Stream.iterate(1, &(&1+1)) |> Enum.take(25)
