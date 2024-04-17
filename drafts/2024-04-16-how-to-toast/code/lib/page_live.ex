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

      <.toast />
      <template id="toast-template">
        <.toast />
      </template>
    </div>
    """
  end

  def toast(assigns) do
    ~H"""
    <div class="fixed left-0 bottom-0 w-full px-8 mb-8">
      <div class="bg-gray-800 text-white px-3 py-2 w-full rounded-sm animate-popup">
        You did it!
      </div>
    </div>
    """
  end
end
