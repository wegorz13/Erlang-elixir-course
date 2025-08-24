defmodule PollutiondbWeb.StationLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      stations: Station.get_all(),
      name: "",
      lat: "",
      lon: "",
      query: ""
    )
    {:ok, socket}
  end

  def handle_event("insert", %{"name" => name, "lat" => lat, "lon" => lon}, socket) do
    Station.add(%Station{name: name, lat: to_float(lat, 0.0), lon: to_float(lon, 0.0)})

    socket = assign(socket, stations: Station.get_all(), name: name, lat: lat, lon: lon)
    {:noreply, socket}
  end

  def handle_event("search", %{"query" => query}, socket) do
    stations =
      case query do
        "" -> Station.get_all()
        _ -> Station.find_by_name(query)
      end

    socket = assign(socket, stations: stations, query: query)
    {:noreply, socket}
  end

  defp to_float(val, default) do
    case Float.parse(val) do
      {fval, ""} -> fval
      _ -> default
    end
  end

  def render(assigns) do
    ~H"""
    Search a station
    <form phx-change="search">
    <input type="text" name="query" value={@query} /><br/>
    </form>

    <table>
        <tr>
        <th>Name</th><th>Longitude</th><th>Latitude</th>
        </tr>
        <%= for station <- @stations do %>
        <tr>
            <td><%= station.name %></td>
            <td><%= station.lon %></td>
            <td><%= station.lat %></td>
        </tr>
        <% end %>
    </table>

    Create new station
    <form phx-submit="insert">
    Name: <input type="text" name="name" value={@name} /><br/>
    Lat: <input type="number" step="any" name="lat" value={@lat} /><br/>
    Lon: <input type="number" step="any" name="lon" value={@lon} /><br/>
    <input type="submit" />
    </form>
    """
  end
end