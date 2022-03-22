defmodule Example do
  use GenServer

  @moduledoc """

  Example

      {:ok, pid} = Example.start_link []
      spawn(fn -> GenServer.stop(pid) end)
      send(pid, "some msg")
  """

  def start_link([]) do
    GenServer.start_link(__MODULE__, [])
  end

  def init([]) do
    Process.flag(:trap_exit, true)
    {:ok, []}
  end

  def terminate(_reason, _state) do
    flush()
    :stop
  end

  def flush() do
    receive do
      msg ->
        IO.puts("Got #{msg}")
        flush()
    end
  end
end
