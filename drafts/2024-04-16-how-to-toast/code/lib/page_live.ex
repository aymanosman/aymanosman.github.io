defmodule Toast.PageLive do
  use Phoenix.LiveView,
    layout: {Toast.Layouts, :app}

  alias Phoenix.LiveView.JS

  @impl true
  def render(assigns) do
    ~H"""
    <div id="page" class="h-screen">
      <button
        phx-click={JS.dispatch("toast:clip", detail: %{text: "Copy this text"})}
        class="animate-pop bg-blue-500 px-3 py-2 rounded-md text-white"
      >
        Copy Link
      </button>
    </div>
    """
  end
end
