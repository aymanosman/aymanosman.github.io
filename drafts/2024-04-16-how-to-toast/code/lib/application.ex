defmodule Toast.Application do
  use Application

  def start(_type, _args) do
    children = [
      {Phoenix.PubSub, name: Toast.PubSub},
      Toast.Endpoint
    ]

    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
