defmodule Pollutiondb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      Pollutiondb.Repo
      # Starts a worker by calling: Pollutiondb.Worker.start_link(arg)
      # {Pollutiondb.Worker, arg}
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Pollutiondb.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
