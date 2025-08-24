defmodule PollutiondbWeb.StationRangeLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      stations: Station.get_all(),
      name: "",
      lat_min: 49,
      lat_max: 51,
      lon_min: 19,
      lon_max: 21
    )
    {:ok, socket}
  end

  def handle_event("update",%{"lat_min" => lat_min, "lat_max" => lat_max, "lon_min" => lon_min, "lon_max" => lon_max},socket) do
    stations =
      Station.find_by_location_range(
        to_float(lon_min, 19),
        to_float(lon_max, 21),
        to_float(lat_min, 49),
        to_float(lat_max, 51)
      )

    socket =
      assign(socket,
        stations: stations,
        lat_min: lat_min,
        lat_max: lat_max,
        lon_min: lon_min,
        lon_max: lon_max
      )

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
    <form phx-change="update">
    Lat min
    <input type="range" min="49" max="51" step="0.001" name="lat_min" value={@lat_min}/><br/>
    Lat max
    <input type="range" min="49" max="51" step="0.001" name="lat_max" value={@lat_max}/><br/>
    Lon min
    <input type="range" min="19" max="21" step="0.001" name="lon_min" value={@lon_min}/><br/>
    Lon max
    <input type="range" min="19" max="21" step="0.001" name="lon_max" value={@lon_max}/><br/>
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
    """
  end
end