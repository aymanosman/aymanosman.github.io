import Config

port =
  case config_env() do
    :test -> 4002
    _ -> 4000
  end

config :toast, Toast.Endpoint,
  server: true,
  url: [host: "localhost"],
  adapter: Bandit.PhoenixAdapter,
  render_errors: [
    formats: [html: Toast.ErrorHTML, json: Toast.ErrorJSON],
    layout: false
  ],
  http: [port: port],
  secret_key_base: "6eHCc3Lj/4XbS9lzcpi8PbS6axE0Ycv2PfTPgcZZTEsabYkS289NsNG4BikCzWV/",
  live_view: [signing_salt: "F8bLAINDFWGMAQAm"]

if config_env() == :test do
  config :logger, level: :warning
end
