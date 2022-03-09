Mix.install([:mox])

defmodule Weather do
  @callback temp(MyApp.LatLong.t()) :: {:ok, integer()}
  @callback humidity(MyApp.LatLong.t()) :: {:ok, integer()}

  def temp(lat_long), do: impl().temp(lat_long)
  def humidity(lat_long), do: impl().humidity(lat_long)
  defp impl, do: Application.get_env(:weather, :weather_impl, ExternalWeather)
end

defmodule HumanizedWeather do
  def display_temp({lat, long}) do
    {:ok, temp} = Weather.temp({lat, long})
    "Current temperature is #{temp} degrees"
  end

  def display_humidity({lat, long}) do
    {:ok, humidity} = Weather.humidity({lat, long})
    "Current humidity is #{humidity}%"
  end
end

Mox.defmock(MockWeather, for: Weather)
Application.put_env(:weather, :weather_impl, MockWeather)

ExUnit.start()

defmodule HumanizedWeatherTest do
  use ExUnit.Case, async: true

  import Mox

  setup :verify_on_exit!

  test "gets and formats temperature and humidity" do
    MockWeather
    |> expect(:temp, fn {_lat, _long} -> {:ok, 30} end)
    |> expect(:humidity, fn {_lat, _long} -> {:ok, 60} end)

    assert HumanizedWeather.display_temp({50.06, 19.94}) ==
             "Current temperature is 30 degrees"

    assert HumanizedWeather.display_humidity({50.06, 19.94}) ==
             "Current humidity is 60%"
  end
end
