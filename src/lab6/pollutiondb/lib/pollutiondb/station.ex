require Ecto.Query

defmodule Pollutiondb.Station do
  use Ecto.Schema

  schema "stations" do
    field :name, :string
    field :lon, :float
    field :lat, :float
    has_many :readings, Pollutiondb.Reading
  end


  def add(station) do
    Pollutiondb.Repo.insert(station)
  end

  def create_new_station(name, lon, lat) do
    %Pollutiondb.Station{}
    |> validate(%{name: name, lon: lon, lat: lat})
    |> Pollutiondb.Repo.insert()
  end

  defp validate(station, changesmap) do
    station
    |> Ecto.Changeset.cast(changesmap, [:name, :lon, :lat])
    |> Ecto.Changeset.validate_required([:name, :lon, :lat])
    |> Ecto.Changeset.validate_number(
         :lon, greater_than: -200.0, less_than: 200)
    |> Ecto.Changeset.validate_number(
         :lat, greater_than: -100.0, less_than: 100)
  end


  def get_all() do
    Pollutiondb.Repo.all(Pollutiondb.Station)
  end

  def get_by_id(id) do
    Pollutiondb.Repo.get(Pollutiondb.Station, id)
  end

  def remove(station) do
    Pollutiondb.Repo.delete(station)
  end

  def find_by_name(name) do
    Pollutiondb.Repo.all(
      Ecto.Query.where(Pollutiondb.Station, name: ^name) )
  end

  def find_by_location(lon, lat) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon == ^lon,
      where: s.lat == ^lat)
    |> Pollutiondb.Repo.all
  end

  def find_by_location_range(lon_min, lon_max, lat_min, lat_max) do
    Ecto.Query.from(s in Pollutiondb.Station,
      where: s.lon >= ^lon_min,
      where: s.lon <= ^lon_max,
      where: s.lat >= ^lat_min,
      where: s.lat <= ^lat_max
    )
    |> Pollutiondb.Repo.all
  end

  def update_name([station], newname) do
    Ecto.Changeset.cast(station, %{name: newname}, [:name])
    |> Ecto.Changeset.validate_required([:name])
    |> Ecto.Changeset.validate_length(:name, 2)
    |> Pollutiondb.Repo.update
  end

  def update_location(station, lon, lat) do
    Ecto.Changeset.cast(station, %{lon: lon, lat: lat}, [:lat, :lon])
    |> Ecto.Changeset.validate_required([:lon, :lat])
    |> Ecto.Changeset.validate_number(:lat, greater_than_or_equal: -90)
    |> Ecto.Changeset.validate_number(:lat, less_than_or_equal: 90)
    |> Ecto.Changeset.validate_number(:lon, greater_than_or_equal: -180)
    |> Ecto.Changeset.validate_number(:lon, less_than_or_equal: 180)
    |> Pollutiondb.Repo.update
  end
end