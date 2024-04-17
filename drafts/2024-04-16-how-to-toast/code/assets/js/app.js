import {Socket} from "phoenix"
import {LiveSocket} from "phoenix_live_view"

let csrfToken = document.querySelector("meta[name='csrf-token']").getAttribute("content")
let liveSocket = new LiveSocket("/live", Socket, {
  params: {_csrf_token: csrfToken}
})

liveSocket.connect()

window.liveSocket = liveSocket

const toast = {
  create: function(message) {
    let toast = document.createElement("toast-message");
    toast.setAttribute("message", message);
    document.body.appendChild(toast);
  }
}

window.addEventListener("toast:clip", (event) => {
  navigator.clipboard.writeText(event.detail.text);
  toast.create("Copied to clipboard.")
});
