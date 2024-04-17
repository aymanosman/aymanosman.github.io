defmodule Toast.Endpoint do
  use Phoenix.Endpoint, otp_app: :toast

  @session_options [
    store: :cookie,
    key: "_toast_key",
    signing_salt: "secret",
    same_site: "Lax"
  ]

  socket "/live", Phoenix.LiveView.Socket, websocket: [connect_info: [session: @session_options]]

  plug Toast.Router
end
