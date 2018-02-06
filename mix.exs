defmodule Cuttlefish.Mixfile do
  use Mix.Project

  @compile :nowarn_unused_vars

  def project do
    [app: :cuttlefish,
     version: "0.1.0",
     elixir: "~> 1.5.2",
     description: description(),
     escript: escript(),
     deps: dependencies() ,
     language: :erlang 
    ]
  end

  defp dependencies() do
    [
      {:lager, "~> 3.6"},
      {:getopt, [ git:   "https://github.com/basho/getopt.git", branch: "rebar3" ] }
    ]
  end

  defp escript do
    [main_module: :cuttlefish_escript, 
     app: nil,
     applications: [:getopt , :lager, :cuttlefish ]
    ]
  end


  def application do
     [
     ]
  end

  #defp package do
  #  [files: ~w(src lib mix.exs priv rebar.config README.textile LICENSE VERSION),
  #   contributors: ["Foo"],
  #   licenses: ["Foo license"],
  #   links: %{"GitHub" => "https://github.com/fooo.... "}]
  #end

  defp description do
    """
    Erlang library and packrat parser-generator for parsing expression grammars.
    """
  end

end
