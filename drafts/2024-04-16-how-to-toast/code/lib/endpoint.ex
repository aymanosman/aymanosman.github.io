defmodule Toast.Endpoint do
  use Phoenix.Endpoint, otp_app: :toast

  @session_options [
    store: :cookie,
    key: "_toast_key",
    signing_salt: "secret",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]]

  plug Plug.Static,
    at: "/",
    from: :toast,
    gzip: false

  if code_reloading? do
    plug Phoenix.CodeReloader
  end

  if code_reloading? do
    socket "/phoenix/live_reload/socket", Phoenix.LiveReloader.Socket
    plug Phoenix.LiveReloader
  end

  plug Plug.Session, @session_options

  plug Toast.Router
end
