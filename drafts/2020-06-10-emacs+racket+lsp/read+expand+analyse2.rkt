#lang racket

;; read - empty

(parameterize ([read-accept-reader #t]
               [port-count-lines-enabled #t])
  (read-syntax "" (open-input-string "")))

(parameterize ([read-accept-reader #t]
               [port-count-lines-enabled #t])
  (read-syntax "" (open-input-string "#lang racket/base
(require jon)")))

;; expand - empty

(parameterize ([read-accept-reader #t]
               [port-count-lines-enabled #t]
               [current-namespace (make-base-namespace)])
  (with-handlers ()
    (expand (read-syntax "" (open-input-string "")))))

;; expand - module #lang

(parameterize ([read-accept-reader #t]
               [port-count-lines-enabled #t]
               [current-namespace (make-base-namespace)])
  (with-handlers ([exn:fail:filesystem:missing-module?
                   (lambda (exn) exn)])
    (expand (read-syntax "" (open-input-string "#lang foo")))))

;; expand - module

(parameterize ([read-accept-reader #t]
               [port-count-lines-enabled #t]
               [current-namespace (make-base-namespace)])
  (with-handlers ([exn:fail:filesystem:missing-module?
                   (lambda (exn) exn)])
    (expand (read-syntax "" (open-input-string "#lang racket/base
(require jon)")))))