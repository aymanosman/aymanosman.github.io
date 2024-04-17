defmodule Toast.Layouts do
  use Phoenix.VerifiedRoutes,
    router: Toast.Router,
    endpoint: Toast.Endpoint,
    statics: ~w(assets fonts images favicon.ico robots.txt)

  import Phoenix.Component

  def root(assigns) do
    ~H"""
    <!DOCTYPE html>
    <html lang="en">
      <head>
        <meta charset="utf-8" />
        <meta name="viewport" content="width=device-width, initial-scale=1" />
        <.live_title><%= assigns[:page_title] || "Toast" %></.live_title>
        <link rel="stylesheet" href={~p"/assets/app.css"} />
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
