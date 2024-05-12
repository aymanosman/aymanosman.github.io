(defvar *html-pretty* t)
(defvar *html-stream* (make-synonym-stream '*standard-output*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; https://developer.mozilla.org/en-US/docs/Glossary/Void_element
  (defvar *html-void-elements* '("area" "base" "br" "col" "embed" "hr" "img"
                                 "input" "link" "meta" "source" "track" "wbr"))

  (defun %html-escape (string)
    (flet ((write-escape (string)
             (format *html-stream* "&~A;" string)))
      (with-output-to-string (output)
        (loop for char across string
              do (case char
                   (#\< (write-escape "lt"))
                   (#\> (write-escape "gt"))
                   (otherwise
                    (write-char char *html-stream*))))))))

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
