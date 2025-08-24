defmodule Pollutiondb.Application do
  # See https://hexdocs.pm/elixir/Application.html
  # for more information on OTP Applications
  @moduledoc false

  use Application

  @impl true
  def start(_type, _args) do
    children = [
      # Start the Telemetry supervisor
      PollutiondbWeb.Telemetry,
      # Start the PubSub system
      {Phoenix.PubSub, name: Pollutiondb.PubSub},
      # Start the Endpoint (http/https)
      PollutiondbWeb.Endpoint,
      # Start a worker by calling: Pollutiondb.Worker.start_link(arg)
      # {Pollutiondb.Worker, arg}
      Pollutiondb.Repo
    ]

    # See https://hexdocs.pm/elixir/Supervisor.html
    # for other strategies and supported options
    opts = [strategy: :one_for_one, name: Pollutiondb.Supervisor]
    Supervisor.start_link(children, opts)
  end

  # Tell Phoenix to update the endpoint configuration
  # whenever the application is updated.
  @impl true
  def config_change(changed, _new, removed) do
    PollutiondbWeb.Endpoint.config_change(changed, removed)
    :ok
  end
end
