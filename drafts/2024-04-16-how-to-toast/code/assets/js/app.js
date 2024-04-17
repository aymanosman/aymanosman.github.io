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
    toast.setAttribute("timeout", 1500);
    document.body.appendChild(toast);
  }
}

class ToastElement extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    this.appendChild(renderTemplate({message: this.getAttribute("message")}));
    let timeout = parseInt(this.getAttribute("timeout") || "1500", 10);
    setTimeout(() => {
      this.hide();
    }, timeout);
  }

  hide() {
    let element = this.querySelector("[data-toast]");
    element.classList.remove("animate-popup");
    element.classList.add("animate-fadedown");
    element.addEventListener("animationend", () => {
      this.remove();
    });
  }
}

customElements.define('toast-message', ToastElement);

window.addEventListener("toast:clip", (event) => {
  navigator.clipboard.writeText(event.detail.text);
  toast.create("Copied to clipboard.")
});

function renderTemplate(assigns) {
  let template = document.getElementById("toast-template");
  let element = template.content.cloneNode(true);
  let slot = element.querySelector('slot[name=message]');
  slot.replaceWith(assigns.message);
  return element;
}
