import Config

config :toast, Toast.Endpoint,
  server: true,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [formats: [html: Toast.ErrorHTML, json: Toast.ErrorJSON]],
  http: [port: 4000],
  secret_key_base: "6eHCc3Lj/4XbS9lzcpi8PbS6axE0Ycv2PfTPgcZZTEsabYkS289NsNG4BikCzWV/",
  live_view: [signing_salt: "F8bLAINDFWGMAQAm"]
