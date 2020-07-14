#lang racket

(define-syntax-rule (define-state nested parent clause* ...)
  (define (nested msg)
    (match msg
      clause* ...
      [_
       (define-values (reply new-state) (parent msg))
       (values reply (cond
                       [(equal? new-state parent) nested]
                       [else new-state]))])))

(struct request (method text) #:transparent)

(define (init msg)
  (match msg
    [(request "init" _)
     (values 'ok accept:analyze)]))

(define (accept msg)
  (match msg
    #;
    ['enter
     (values #f accept:read)]
    [(request "shutdown" _)
     (values 'ok shutdown)]))

(define (shutdown msg)
  (match msg
    [(request "exit" _)
     (values #f 'exit)]))

(define-state accept:read accept
  [(request (or "didChange" "didOpen") text)
   (read+expand+analyze text)])

(define-state accept:expand accept
  [(request (or "didChange" "didOpen") text)
   (read+expand+analyze text)])

(define-state accept:analyze accept
  [(request (or "didChange" "didOpen") text)
   (read+expand+analyze text)])

(define (analyze stx)
  (values (list (hash 'message "unused"))
          accept:analyze))

(define (handle-read-error exn)
  (values (list (hash 'message (exn-message exn)))
          accept:read))

(define (handle-syntax-error exn)
  (values (list (hash 'message (exn-message exn)))
          accept:expand ))

(define (read+expand+analyze text)
  (parameterize ([read-accept-reader #t]
                 [port-count-lines-enabled #t]
                 [current-namespace (make-base-namespace)])
    (with-handlers ([exn:fail:read? handle-read-error]
                    [exn:fail:filesystem? handle-read-error]
                    [exn:fail:syntax? handle-syntax-error])
      (analyze
       (expand
        (read-syntax "dummy-file.rkt" (open-input-string text)))))))
