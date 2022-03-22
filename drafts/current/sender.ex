defmodule Sender do
  def child_spec(_opts) do
    Task.Supervisor.child_spec(name: __MODULE__)
  end

  def send_email(email) do
    Task.Supervisor.async(
      __MODULE__,
      fn ->
        Process.sleep(1000)
        IO.puts("Email to #{email} sent")
        {:ok, "email_sent"}
      end
    )
    |> Task.await()
  end
end

# Task.Supervisor.start_link(name: Sender)
