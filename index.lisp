(defvar *html-pretty* t)
(defvar *html-stream* (make-synonym-stream '*standard-output*))

(defun html-escape (string)
  (flet ((write-escape (string)
           (format *html-stream* "&~A;" string)))
    (with-output-to-string (output)
      (loop for char across string
            do (case char
                 (#\< (write-escape "lt"))
                 (#\> (write-escape "gt"))
                 (otherwise
                  (write-char char *html-stream*)))))))

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

(defun write-element (name attrs content &key (stream *html-stream*) (pretty *html-pretty*))
  (format stream "<~A" name)
  (write-attrs attrs)
  (format stream ">")
  (when pretty (terpri stream))
  (cond
    ((member name '("link") :test 'equal)
     (unless (null content)
       (warn "link element should not have a body")))
    (t
     (when content
       (funcall content))
     (format stream "</~A>" name)
     (when pretty (terpri stream)))))

(defmacro with-element ((name &rest attrs) &body body)
  (check-type name string)
  `(write-element ,name (list ,@attrs)
                  ,(cond
                     ((member name '("link") :test 'equal)
                      nil)
                     (t
                      `(lambda () ,@body)))))

(defun write-index ()
  (format *html-stream* "<!doctype html>~%")
  (with-element ("html")
    (with-element ("head")
      (with-element ("link" :rel "preconnect" :href "https://fonts.googleapis.com"))
      (with-element ("link" :rel "preconnect" :href "https://fonts.gstatic.com" :crossorigin t))
      (with-element ("link" :rel "stylesheet" :href "https://fonts.googleapis.com/css2?family=Crimson+Text:ital,wght@0,400;0,600;0,700;1,400;1,600;1,700&display=swap"))
      (with-element ("link" :rel "stylesheet" :href "styles.css "))
      (with-element ("link" :rel "stylesheet" :href "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/styles/default.min.css"))
      (with-element ("script" :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/highlight.min.js"))
      (with-element ("script" :src "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/11.9.0/languages/elixir.min.js"))
      (with-element ("script")
        (write-string "hljs.highlightAll();" *html-stream*)))
    (with-element ("body")
      (format *html-stream* "Hello World~%")
      (let ((*html-pretty* nil))
        (with-element ("pre")
          (with-element ("code" :class "language-elixir")
            (write-string (html-escape "def button (assigns) do ~H\"<span>Hello HEEx</span>\"")
                          *html-stream*)))))))
