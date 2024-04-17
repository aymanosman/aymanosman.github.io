(defvar *html-pretty* t)
(defvar *html-stream* (make-synonym-stream '*standard-output*))

(defun %html-escape (string)
  (flet ((write-escape (string)
           (format *html-stream* "&~A;" string)))
    (with-output-to-string (output)
      (loop for char across string
            do (case char
                 (#\< (write-escape "lt"))
                 (#\> (write-escape "gt"))
                 (otherwise
                  (write-char char *html-stream*)))))))

(defun html-escape (string)
  (%html-escape string))

(define-compiler-macro html-escape (form)
  (typecase form
    (string (with-output-to-string (*html-stream*)
              (%html-escape form)))
    (otherwise form)))

(defun write-attrs (attrs)
  (labels ((write-attr-name (name)
             (format *html-stream* " ~(~A~)" name))
           (write-attr (name value)
             (case value
               ((t)
                (write-attr-name name))
               ((nil)
                nil)
               (otherwise
                (check-type value string)
                (write-attr-name name)
                (format *html-stream* "=~S" value)))))
    (loop for (name value) on attrs by #'cddr
          do (write-attr name value))))

;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element
(defvar *html-void-elements* '("area" "base" "br" "col" "embed" "hr" "img"
                               "input" "link" "meta" "source" "track" "wbr"))

(defun write-element (name attrs content &key (stream *html-stream*) (pretty *html-pretty*))
  (format stream "<~A" name)
  (write-attrs attrs)
  (format stream ">")
  (when pretty (terpri stream))
  (cond
    ((member name *html-void-elements* :test 'equal)
     (unless (null content)
       (warn "~A void-element should not have a body" name)))
    (t
     (when content
       (funcall content))
     (format stream "</~A>" name)
     (when pretty (terpri stream)))))

(defmacro with-element ((name &rest attrs) &body body)
  (check-type name string)
  `(write-element ,name (list ,@attrs)
                  ,(cond
                     ((member name *html-void-elements* :test 'equal)
                      nil)
                     (t
                      `(lambda ()
                         ,@(mapcar (lambda (child)
                                     (typecase child
                                       (string
                                        `(progn
                                           (write-string (html-escape ,child) *html-stream*)
                                           (when *html-pretty* (terpri *html-stream*))))
                                       (otherwise
                                        child)))
                                   body))))))

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
