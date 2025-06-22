defmodule Measurement do
    defstruct datetime: "", location: "", stationId: "", stationName: "", pollutionType: "", pollutionLevel: ""
end

defmodule PollutionDataLoader do
  def readFile(file_name) do
    File.read!(file_name) |> String.split("\n") |> Enum.reject(&(&1 == ""))
  end

  def parseLine(line) do
    [datetime, type, level, id, name, location] = String.split(line, ";")
    datetime = mapDate(datetime)
    location = mapLocation(location)

    %Measurement{datetime: datetime, location: location, stationId: String.to_integer(id), stationName: name, pollutionType: type, pollutionLevel: String.to_float(level)}
  end

  def mapDate(datetime) do
      date = datetime |> String.slice(0, 10) |> String.split("-") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      time = datetime |> String.slice(11, 8) |> String.split(":") |> Enum.map(&String.to_integer/1) |> List.to_tuple()
      {date, time}
  end

  def mapLocation(location) do
      location |> String.split(",") |> Enum.map(&String.to_float/1) |> List.to_tuple()
  end

  def identifyStations(measurements) do
      measurements |> Enum.map(fn m -> {m.stationId, m.stationName, m.location} end) |> Enum.uniq()
  end

  def insertStations(file_name) do
    stations = reader(file_name) |> Enum.map(&parser/1) |> identifyStations()
    for {stationId, stationName, location} <- stations do
      :pollution_gen_server.add_station("#{stationId} #{stationName}", location)
    end
  end

  def insertData(file_name) do
    measurements = reader(file_name) |> Enum.map(&parser/1)
    for m <- measurements do
      :pollution_gen_server.add_value(m.location, m.datetime, m.pollutionType, m.pollutionLevel)
    end
  end

  def insertData(file_name, type) do
    measurements = reader(file_name) |> Enum.map(&parser/1)
    for m <- measurements do
      :pollution_gen_server.add_value(m.location, m.datetime, type, m.pollutionLevel)
    end
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