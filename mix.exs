defmodule Mulix.MixProject do
  use Mix.Project

  def project do
    [
      app: :mulix,
      version: "0.1.0",
      elixir: "~> 1.6",
      start_permanent: Mix.env() == :prod,
      deps: deps(),

      # Docs
      name: "mulix",
      docs: [main: "Mulix"],
      dialyzer: [
        flags: [:unmatched_returns, :error_handling, :underspecs],
        ignore_warnings: "dialyzer.ignore-warnings"
      ]
    ]
  end

  # Run "mix help compile.app" to learn about applications.
  def application do
    [
      extra_applications: [:logger]
    ]
  end

  # Run "mix help deps" to learn about dependencies.
  defp deps do
    [
      {:ex_doc, "~> 0.16", only: :dev, runtime: false},
      {:witchcraft, "~> 1.0"},
      {:algae, "~> 1.1"},
      {:dialyxir, "~> 0.5", only: [:dev], runtime: false},
      {:credo, "~> 0.9.1", only: [:dev, :test], runtime: false},
      {:mex, "~> 0.0.5", only: :dev},
      {:bitmap, "~> 1.0"},
      {:memoize, "~> 1.2"},
      {:libring, "~> 1.0"}
    ]
  end
end
