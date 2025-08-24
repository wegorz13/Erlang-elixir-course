defmodule Pollutiondb.Reading do
  use Ecto.Schema
  import Ecto.Query

  schema "readings" do
    field :type, :string
    field :datetime, :utc_datetime
    field :value, :float
    belongs_to :station, Pollutiondb.Station
    end

  def changeset(reading, attrs) do
    reading
    |> Ecto.Changeset.cast(attrs, [:station_id, :datetime, :type, :value])
    |> Ecto.Changeset.validate_required([:station_id, :datetime, :type, :value])
  end


  def add(%Pollutiondb.Station{} = station, %DateTime{} = datetime, type, value) do
    %Pollutiondb.Reading{}
    |> changeset(%{
      station_id: station.id,
      datetime: datetime,
      type: type,
      value: value
    })
    |> Pollutiondb.Repo.insert()
  end

  def insert_mock_data() do
    [
      {"Prądnik Biały", "PM10", 121.5},
      {"Prądnik Biały", "PM2.5", 63.5},
      {"Krowodrza Górka", "PM10", 21.5},
      {"Nowa Huta", "PM10", 28.5},
    ]
    |> Enum.map(fn {station_name, type, val} ->
      add_now(station_name |> Pollutiondb.Station.find_by_name(), type, val) end)

    [
      {"Krowodrza Górka", "PM10", 17.5}
    ]
    |> Enum.map(fn {station_name, type, val} ->
      add(%Pollutiondb.Reading{
        station: station_name |> Pollutiondb.Station.find_by_name(),
        type: type, value: val,
        datetime: ~U[2023-06-20 23:11:50Z]
      }) end)
  end

  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Reading)
    |> Pollutiondb.Repo.preload(:station)
  end

  def get_latest() do
    Ecto.Query.from(r in Pollutiondb.Reading,
      limit: 10, order_by: [desc: r.datetime])
    |> Pollutiondb.Repo.all()
    |> Pollutiondb.Repo.preload(:station)
  end

  def add(reading) do
    Pollutiondb.Repo.insert(reading)
  end

  def add_now(station, type, value) do
    add(%Pollutiondb.Reading{
      station: station, type: type, value: value,
      datetime: DateTime.utc_now |> DateTime.truncate(:second)
    })
  end

  def find_by_date(date_string) do
    {:ok, date} = Date.from_iso8601(date_string)

    minDateTime = DateTime.new!(date, ~T[00:00:00])
    maxDateTime = DateTime.add(minDateTime, 24*60*60, :second)

    Ecto.Query.from(r in Pollutiondb.Reading, limit: 10,
      where: r.datetime >= ^minDateTime and r.datetime <= ^maxDateTime
    )
    |> Pollutiondb.Repo.all()
  end
end