defmodule Toast.ErrorHTML do
  def render(template, _assigns) do
    Phoenix.Controller.status_message_from_template(template)
  end
end

defmodule Toast.ErrorJSON do
  def render(template, _assigns) do
    %{errors: %{detail: Phoenix.Controller.status_message_from_template(template)}}
  end
end
