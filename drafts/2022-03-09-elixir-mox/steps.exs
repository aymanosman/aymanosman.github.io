Mix.install([:mox])

defmodule Weather do
  @callback temp({number, number}) :: {:ok, integer()}
  def temp(lat_long), do: impl().temp(lat_long)
  defp impl, do: Application.get_env(:my_app, :weather, ExternalWeather)
end

defmodule HumanizedWeather do
  def display_temp({lat, long}) do
    {:ok, temp} = Weather.temp({lat, long})
    "Current temperature is #{temp} degrees"
  end
end

Mox.defmock(MockWeather, for: Weather)
Application.put_env(:my_app, :weather, MockWeather)

ExUnit.start()

defmodule HumanizedWeatherTest do
  use ExUnit.Case, async: true

  import Mox

  setup :verify_on_exit!

  test "gets and formats temperature and humidity" do
    MockWeather
    |> expect(:temp, fn {_lat, _long} -> {:ok, 30} end)

    assert HumanizedWeather.display_temp({50.06, 19.94}) ==
             "Current temperature is 30 degrees"
  end
end
