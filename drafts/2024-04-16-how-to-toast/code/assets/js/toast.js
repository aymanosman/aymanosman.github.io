class ToastElement extends HTMLElement {
  constructor() {
    super();
  }

  connectedCallback() {
    let template = new Template(document.getElementById("toast-template"));
    this.appendChild(template.render({message: this.getAttribute("message")}));

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

class Template {
  constructor(template) {
    this._template = template;
  }

  render(assigns) {
    let clone = this._template.content.cloneNode(true);
    for (let slot of clone.querySelectorAll("slot")) {
      let name = slot.getAttribute("name");
      if (assigns[name]) {
        slot.replaceWith(assigns[name]);
      }
    }
    return clone;
  }
}

customElements.define('toast-message', ToastElement);

export const toast = {
  create: function(message) {
    let toast = document.createElement("toast-message");
    toast.setAttribute("message", message);
    toast.setAttribute("timeout", 1500);
    document.body.appendChild(toast);
  }
}
