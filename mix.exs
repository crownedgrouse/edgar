defmodule Edgar.Mixfile do
  use Mix.Project

  def project do
    [app: :edgar,
     version: "1.0.2",
     elixir: "~> 1.2",
     description: description(),
     package: package(),
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps()]
  end

  def application do
    [applications: []]
  end

  defp deps() do
    [
    ]
  end

  defp description() do
    "Erlang Does Gnu AR"
  end

  defp package() do
    [
      # These are the default files included in the package
      files: ["src", "doc", "mix.exs", "README.md", "LICENSE", "Makefile", "erlang.mk", "rebar.config"],
      maintainers: ["Eric Pailleau"],
      licenses: ["ISC"],
      links: %{"GitHub" => "https://github.com/crownedgrouse/edgar"}
    ]
  end
end
