defmodule Bob do
  @spec hey(String.t()) :: String.t()
  def hey(input) do
    q = String.trim(input)

    is_shouting =
      String.match?(q, ~r/[[:upper:]]/u) and
        String.match?(q, ~r/^([[:upper:]]|[[:digit:]]|[[:space:]]|[[:punct:]])+[?.!]?$/u)

    is_asking = String.at(q, -1) == "?"

    cond do
      q == "" -> "Fine. Be that way!"
      is_shouting and is_asking -> "Calm down, I know what I'm doing!"
      is_shouting -> "Whoa, chill out!"
      is_asking -> "Sure."
      true -> "Whatever."
    end
  end
end
