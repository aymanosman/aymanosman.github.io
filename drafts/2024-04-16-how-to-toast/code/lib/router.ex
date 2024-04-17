defmodule Toast.Router do
  use Phoenix.Router

  import Phoenix.LiveView.Router

  live "/", Toast.PageLive
end
