defmodule ToastTest do
  use ExUnit.Case

  import Phoenix.ConnTest
  import Phoenix.LiveViewTest

  @endpoint Toast.Endpoint

  test "live view renders in root and app layout" do
    conn = build_conn()

    {:ok, view, _html} = live(conn, "/")

    assert has_element?(element(view, "section#page"))
  end
end
