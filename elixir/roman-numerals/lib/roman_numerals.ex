defmodule RomanNumerals do
  @doc """
  Convert the number to a roman number.
  """
  @symbols %{
    1000 => "M",
    900 => "CM",
    500 => "D",
    400 => "CD",
    100 => "C",
    90 => "XC",
    50 => "L",
    40 => "XL",
    10 => "X",
    9 => "IX",
    5 => "V",
    4 => "IV",
    1 => "I"
  }

  @spec to_word(pos_integer, {pos_integer, String.t()}) :: String.t()
  defp to_word(key, {number, word}) do
    times = Integer.floor_div(number, key)

    case times do
      0 ->
        {number, word}

      _ when key == 1 ->
        {0, word <> String.duplicate(@symbols[key], times)}

      _
      when key / 9 == 1 or rem(key / 9, 10) or
             key / 4 == 1 or rem(key / 4, 10) ->
        {number - key, word <> @symbols[key]}

      _ ->
        {number - times * key, word <> String.duplicate(@symbols[key], times)}
    end
  end

  @spec numeral(pos_integer) :: String.t()
  def numeral(number) do
    @symbols
    |> Map.keys()
    |> Enum.sort()
    |> Enum.reverse()
    |> Enum.reduce({number, ""}, fn key, acc -> to_word(key, acc) end)
    |> Tuple.to_list()
    |> List.last()
  end
end
