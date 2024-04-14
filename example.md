---
title: Example
---
# This is an example.

```elixir
slot :item do
  attr :current, :boolean
end

def step_list(assigns) do
  assigns =
    assign(assigns,
      last_index: length(assigns.item) - 1,
      current_index: Enum.find_index(assigns.item, fn item -> item[:current] end) || -1
    )

  ~H"""
    <ol>
      <li :for={{item, index} <- Enum.with_index(@item)} class="relative pb-10">
        <div
          :if={index != @last_index}
          class={[
            "absolute left-4 top-4 -ml-px mt-0.5 h-full w-0.5",
            cond do
              index < @current_index -> "bg-indigo-600"
              true -> "bg-gray-300"
            end
          ]}
        >
        </div>
        <div class="flex items-start">
          <.step_list_bullet status={
            cond do
              index < @current_index -> :complete
              index == @current_index -> :current
              true -> :upcoming
            end
          } />
          <!-- item content -->
          <span class="ml-4">
            <%= render_slot(item) %>
          </span>
        </div>
      </li>
    </ol>
  """
end

attr :status, :atom, values: [:complete, :current, :upcoming]

defp step_list_bullet(assigns) do
  case assigns.status do
    :complete ->
      ~H"""
      <span class="z-10 h-8 w-8 flex items-center justify-center rounded-full bg-indigo-600">
        <svg class="h-5 w-5 text-white" viewBox="0 0 20 20" fill="currentColor" aria-hidden="true">
          <path
            fill-rule="evenodd"
            d="M16.704 4.153a.75.75 0 01.143 1.052l-8 10.5a.75.75 0 01-1.127.075l-4.5-4.5a.75.75 0 011.06-1.06l3.894 3.893 7.48-9.817a.75.75 0 011.05-.143z"
            clip-rule="evenodd"
          />
        </svg>
      </span>
      """

    :current ->
      ~H"""
      <span class="z-10 h-8 w-8 flex items-center justify-center rounded-full border-2 border-indigo-600 bg-white"></span>
      """

    :upcoming ->
      ~H"""
      <span class="z-10 h-8 w-8 flex items-center justify-center rounded-full border-2 border-gray-300 bg-white"></span>
      """
  end
end
```

```heex
<.step_list>
  <:item>
    <span class="flex flex-col">
      <span class="text-sm font-medium">Get ingredients</span>
      <span class="text-sm text-gray-500">Eggs, flour, etc.</span>
    </span>
  </:item>
  <:item current>
    <span class="flex flex-col">
      <span class="text-sm font-medium">Make cake</span>
      <span class="text-sm text-gray-500">Mix it up. Put it in oven.</span>
    </span>
  </:item>
  <:item>
    <span class="flex flex-col">
      <span class="text-sm font-medium">Eat it</span>
      <span class="text-sm text-gray-500">Open mouth. Put it in mouth.</span>
    </span>
  </:item>
</.step_list>
```

```diff
-         <!-- bullet point -->
-         <span class="z-10 h-8 w-8 rounded-full border-2 border-gray-300 bg-white"></span>
+         <.step_list_bullet status={
+           cond do
+             index < @current_index -> :complete
+             index == @current_index -> :current
+             true -> :upcoming
+           end
+         } />
```
