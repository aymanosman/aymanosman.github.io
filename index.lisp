(defun write-index ()
  (format *html-stream* "<!doctype html>~%")
  (with-element ("html" :lang "en")
    (with-element ("head")
      (with-element ("meta" :charset "utf-8"))
      (with-element ("meta" :http-equiv "x-ua-compatible" :content "ie=edge"))
      (with-element ("meta" :name "viewport" :content "width=device-width, initial-scale=1.0"))

      (with-element ("title")
        "Index")

      (with-element ("link" :rel "preconnect" :href "https://fonts.googleapis.com"))
      (with-element ("link" :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t))
      (with-element ("link" :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Crimson+Text:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap"))
      (with-element ("link" :rel "stylesheet" :href "styles.css "))
      (with-element ("link" :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/default.min.css"))
      (with-element ("script" :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"))
      (with-element ("script" :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/elixir.min.js"))
      (with-element ("script")
        "hljs.highlightAll();"))
    (with-element ("body")
      "Hello World"
      (let ((*html-pretty* nil))
        (with-element ("pre")
          (with-element ("code" :class "language-elixir")
            "def button(assigns) do
  ~H\"\"\"
  <span>Hello HEEx</span>
  \"\"\"
end"))))))
