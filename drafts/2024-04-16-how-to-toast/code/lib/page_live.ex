defmodule Toast.PageLive do
  use Phoenix.LiveView,
    layout: {Toast.Layouts, :app}

  @impl true
  def render(assigns) do
    ~H"""
    <section id="page">
      content goes here
    </section>
    """
  end
end
