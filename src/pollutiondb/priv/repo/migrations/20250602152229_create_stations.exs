defmodule Pollutiondb.Repo.Migrations.CreateStations do
  use Ecto.Migration

  def change do
    create table(:stations) do
      add :name, :string
      add :lon, :float
      add :lat, :float
    end
  end
end
