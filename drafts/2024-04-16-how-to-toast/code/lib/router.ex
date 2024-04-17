defmodule Toast.Router do
  use Phoenix.Router

  import Phoenix.LiveView.Router

  pipeline :browser do
    plug :put_root_layout, {Toast.Layouts, :root}
  end

  scope "/" do
    pipe_through :browser

    live "/", Toast.PageLive
  end
end
