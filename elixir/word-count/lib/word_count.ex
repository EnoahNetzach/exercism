defmodule WordCount do
  @doc """
  Count the number of words in the sentence.

  Words are compared case-insensitively.
  """
  defp count_word(dict, word) do
    if dict[word] != nil do
      dict[word] + 1
    else
      1
    end
  end

  @spec count(String.t()) :: map
  def count(sentence) do
    sentence
    |> String.split(~r{[ ,:_!&@$%^]+}, trim: true)
    |> Enum.reduce(%{}, fn word, acc ->
      low = String.downcase(word)

      Map.merge(acc, %{low => count_word(acc, low)})
    end)
  end
end
