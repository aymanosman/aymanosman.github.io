defmodule Toast.MixProject do
  use Mix.Project

  def project do
    [
      app: :toast,
      version: "0.1.0",
      deps: deps()
    ]
  end

  def application do
    [
      mod: {Toast.Application, []}
    ]
  end

  defp deps do
    [
      {:jason, "~> 1.0"},
      {:plug, "~> 1.0"},
      {:bandit, "~> 1.0"},
      {:phoenix, "~> 1.0"},
      {:phoenix_html, "~> 4.0"},
      {:phoenix_live_view, "~> 0.0"},
      {:phoenix_live_reload, "~> 1.0"},
      {:floki, "~> 0.0", only: :test},
      {:esbuild, "~> 0.0", runtime: Mix.env() == :dev},
      {:tailwind, "~> 0.0", runtime: Mix.env() == :dev}
    ]
  end
end
