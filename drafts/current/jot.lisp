(defpackage #:jot
  (:use #:common-lisp))

(in-package #:jot)

(defstruct token type lexeme)

(defun tokenize (source)
  (let ((pos 0)
        (len (length source))
        (tokens '()))
    (labels ((peek ()
               (if (< pos len)
                   (aref source pos)
                   nil))
             (main ()
               (cond
                 ((< pos len)
                  (push (read-token) tokens)
                  (main))
                 (t
                  (reverse tokens))))
             (read-token ()
               (case (peek)
                 (#\{
                  (advance)
                  (make-token :type :open-brace))
                 (#\}
                  (advance)
                  (make-token :type :close-brace))
                 (#\:
                  (advance)
                  (read-atom))
                 (#\,
                  (advance)
                  (make-token :type :comma))
                 ((#\Space #\Newline)
                  (read-whitespace))
                 (#\<
                  (advance)
                  (case (peek)
                    (#\<
                     (advance)
                     (make-token :type :open-binary))
                    (otherwise
                     (error "TODO handle < operators"))))
                 (#\>
                  (advance)
                  (case (peek)
                    (#\>
                     (advance)
                     (make-token :type :close-binary))
                    (otherwise
                     (error "TODO handle > operators"))))
                 (#\.
                  (advance)
                  (consume #\. "expected .")
                  (consume #\. "expected .")
                  (make-token :type :elipses))
                 (otherwise
                  (cond
                    ((alpha-char-p (peek))
                     (read-atom))
                    ((digit-char-p (peek))
                     (read-number))
                    (t
                     (error (format nil "unexpected character ~s" (peek))))))))
             (advance ()
               (incf pos))
             (consume (char message)
               (unless (char= (peek) char)
                 (error message))
               (advance))
             (read-atom ()
               (let ((start pos))
                 (loop :while (alpha-char-p (peek))
                       :do (advance))
                 (make-token :type :atom :lexeme (subseq source start pos))))
             (read-number ()
               (let ((start pos))
                 (loop :while (digit-char-p (peek))
                       :do (advance))
                 (make-token :type :number :lexeme (subseq source start pos))))
             (read-whitespace ()
               (let ((start pos))
                 (loop :while (whitespace-p (peek))
                       :do (advance))
                 (make-token :type :whitespace :lexeme (subseq source start pos)))))
      (main))))

(defun whitespace-p (char)
  (member char '(#\Space #\Newline)))

(defun example (&optional (filespec "question.jot"))
  (let ((tokens (tokenize (read-file filespec))))
    (with-open-file (stream "example.html" :direction :output :if-exists :supersede)
      (format stream "<!doctype html>")
      (format stream "<html>")
      (format stream "<body>")
      (format stream "<pre style=\"background-color: black;\">")
      (format stream "<code>")
      (render-html tokens :stream stream)
      (format stream "</code>")
      (format stream "</pre>")
      (format stream "</body>")
      (format stream "</html>"))))

(defun write-span (stream style content)
  (format stream "<span style=\"color:~a;\">~a</span>" style (spinneret::escape-string content)))

(defun render-html (tokens &key (stream t))
  (dolist (token tokens)
    (ecase (token-type token)
      (:open-brace
       (write-span stream "cyan" "{"))
      (:close-brace
       (write-span stream "cyan" "}"))
      (:open-binary
       (write-span stream "blue" "<<"))
      (:close-binary
       (write-span stream "blue" ">>"))
      (:elipses
       (write-span stream "pink" "..."))
      (:comma
       (write-span stream "white" ","))
      (:atom
       (write-span stream "magenta" (format nil ":~a" (token-lexeme token))))
      ((:whitespace :number)
       (write-span stream "yellow" (token-lexeme token))))))

(defun render (tokens &key (stream t))
  (dolist (token tokens)
    (ecase (token-type token)
      (:open-brace
       (format stream "{"))
      (:close-brace
       (format stream "}"))
      (:open-binary
       (format stream "<<"))
      (:close-binary
       (format stream ">>"))
      (:elipses
       (format stream "..."))
      (:comma
       (format stream ","))
      (:atom
       (format stream ":~a" (token-lexeme token)))
      ((:whitespace :number)
       (format stream "~a" (token-lexeme token))))))

(defun read-file (infile)
  (with-open-file (instream infile :direction :input :if-does-not-exist nil)
    (when instream
      (let ((string (make-string (file-length instream))))
        (read-sequence string instream)
        string))))

(defvar *source*
  (read-file "question.jot"))
