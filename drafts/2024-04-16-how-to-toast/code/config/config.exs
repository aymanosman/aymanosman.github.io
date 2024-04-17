import Config

if config_env() == :dev do
  config :toast, Toast.Endpoint,
    http: [ip: {127, 0, 0, 1}, port: 4000],
    check_origin: false,
    code_reloader: true,
    debug_errors: true,
    live_reload: [
      patterns: [
        ~r"priv/static/.*(js|css)$",
        ~r"lib/.*(ex)$"
      ],
      web_console_logger: true
    ],
    watchers: [
      esbuild: {Esbuild, :install_and_run, [:toast, ~w(--sourcemap=inline --watch)]}
    ]
end

config :esbuild,
  version: "0.17.11",
  toast: [
    args:
      ~w(js/app.js --bundle --target=es2017 --outdir=../priv/static/assets --external:/fonts/* --external:/images/*),
    cd: Path.expand("../assets", __DIR__),
    env: %{"NODE_PATH" => Path.expand("../deps", __DIR__)}
  ]
