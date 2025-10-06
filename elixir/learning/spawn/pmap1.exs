defmodule Parallel do
  def pmap(collection, func) do
    collection
    |> Enum.map(&(Task.async(fn -> func.(&1) end)))
    |> Enum.map(&Task.await/1)
  end
end

# For example
# iex(2)> Parallel.pmap(1..10, &(&1 * &1))
# [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]