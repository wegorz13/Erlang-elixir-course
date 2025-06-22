defmodule Pollutiondb.Repo.Migrations.CreateReadings do
  use Ecto.Migration

  def change do
    create table(:readings) do
      add :type, :string
      add :value, :float
      add :datetime, :utc_datetime
      add :station_id, references(:stations, on_delete: :delete_all)
  end
    create index(:readings, [:station_id])
  end
end
