defmodule PollutiondbWeb.PageController do
  use PollutiondbWeb, :controller

  def index(conn, _params) do
    render(conn, "index.html")
  end
end
