defmodule Reading do
    defstruct datetime: "", location: "", stationId: "", stationName: "", pollutionType: "", pollutionLevel: ""
end

defmodule Pollutiondb.DataLoader do
  def readFile(file_name) do
    File.read!(file_name) |> String.split("\n") |> Enum.reject(&(&1 == ""))
  end

  def parse_line(line) do
    [datetime_str, type, level, id, name, location] = String.split(line, ";")
    {:ok, datetime, _} = DateTime.from_iso8601(datetime_str)
    location = map_location(location)

    %Reading{datetime: datetime, location: location, stationId: String.to_integer(id), stationName: name, pollutionType: type, pollutionLevel: String.to_float(level)}
  end

  def map_date(datetime) do
      date = datetime |> String.slice(0, 10) |> String.split("-") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      time = datetime |> String.slice(11, 8) |> String.split(":") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      {date, time}
  end

  def map_location(location) do
      location |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple()
  end

  def identify_stations(readings) do
    readings |> Enum.map(fn r -> {r.stationId, r.stationName, r.location} end) |> Enum.uniq()
  end

  def insert_all(file_name) do
    readings = readFile(file_name) |> Enum.map(&parse_line/1)

    readings
    |> identify_stations()
    |> Enum.each(fn {_id, name, {lat, lon}} ->
      case Pollutiondb.Station.find_by_name(name) do
        [] -> Pollutiondb.Station.create_new_station(name, lon, lat)
        _ -> :ok
      end
    end)

    Enum.each(readings, fn r ->
      [station] = Pollutiondb.Station.find_by_name(r.stationName)
      Pollutiondb.Reading.add(station, r.datetime, r.pollutionType, r.pollutionLevel)
    end)
  end

  defmodule Timer do
    def get_time_in_sec_and_result(function) do
      {time_micro_s, result} = function |> :timer.tc()
      {time_micro_s * 10 ** -6, result}
    end

    def measure_time_in_sec(function) do
      function |> get_time_in_sec_and_result |> elem(0)
    end
  end

end