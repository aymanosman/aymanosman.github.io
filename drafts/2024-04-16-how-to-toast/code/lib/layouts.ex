defmodule Toast.Layouts do
  use Phoenix.VerifiedRoutes,
    router: Toast.Router,
    endpoint: Toast.Endpoint,
    statics: ~w(assets fonts images favicon.ico robots.txt)

  import Phoenix.Component

  import Phoenix.Controller, only: [get_csrf_token: 0]

  def root(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <meta name="csrf-token" content={get_csrf_token()} />
        <.live_title><%= assigns[:page_title] || "Toast" %></.live_title>
        <link phx-track-static rel="stylesheet" href={~p"/assets/app.css"} />
        <script phx-track-static defer type="text/javascript" src={~p"/assets/app.js"}>
        </script>
      </head>
      <body>
        <%= @inner_content %>
      </body>
    </html>
    """
  end

  def app(assigns) do
    ~H"""
    <main>
      <%= @inner_content %>
    </main>
    """
  end
end
