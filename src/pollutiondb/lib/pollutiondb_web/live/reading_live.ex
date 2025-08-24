defmodule PollutiondbWeb.ReadingLive do
  use PollutiondbWeb, :live_view

  alias Pollutiondb.Reading
  alias Pollutiondb.Station

  def mount(_params, _session, socket) do
    socket = assign(socket,
      readings: Reading.get_latest(),
      date: Date.utc_today() |> Date.to_string(),
      stations: Station.get_all(),
      type: "",
      value: "",
      station_id: nil
    )
    {:ok, socket}
  end

  def handle_event("search", %{"date" => date}, socket) do
    readings =
      case date do
        "" -> Reading.get_latest()
        _ -> Reading.find_by_date(date)
      end

    socket = assign(socket, readings: readings, date: date)
    {:noreply, socket}
  end

  def handle_event("insert", %{"station_id" => id, "type" => type, "value" => val}, socket) do
    station = %Station{id: to_int(id, 1)}
    Reading.add_now(station, type, to_float(val, 0.0))

    {:noreply, assign(socket, readings: Reading.get_latest(), type: "", value: "", station_id: nil)}
  end

  defp to_float(val, default) do
    case Float.parse(val) do
      {fval, ""} -> fval
      _ -> default
    end
  end

  defp to_int(val, default) do
    case Integer.parse(val) do
      {fval, ""} -> fval
      _ -> default
    end
  end

  def render(assigns) do
    ~H"""
    Filter by date
    <form phx-change="search">
    <input type="date" name="date" value={@date} /><br/>
    </form>

    <table>
        <tr>
        <th>Station name</th><th>Type</th><th>Value</th><th>Datetime</th>
        </tr>
        <%= for reading <- @readings do %>
        <tr>
            <td><%= reading.station.name %></td>
            <td><%= reading.type %></td>
            <td><%= reading.value %></td>
            <td><%= reading.datetime %></td>
        </tr>
        <% end %>
    </table>

    Add reading
    <form phx-submit="insert">
      <label>Station:
        <select name="station_id">
          <%= for s <- @stations do %>
            <option value={s.id} selected={s.id == @station_id}><%= s.name %></option>
          <% end %>
        </select>
      </label><br/>
      <label>Type: <input type="text" name="type" value={@type}/></label><br/>
      <label>Value: <input type="number" name="value" step="0.1" value={@value}/></label><br/>
      <button type="submit">Add</button>
    </form>
    """
  end
end