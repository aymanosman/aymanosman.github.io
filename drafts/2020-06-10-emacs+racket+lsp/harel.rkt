#lang racket

(require (for-syntax syntax/parse))

(define-syntax (define-machine stx)
  (syntax-parse stx
    [(define-machine:id name:id (~optional parent:id)
       clause* ...)
     #'(define (name msg)
         (match msg
           clause* ...
           ['enter (values #f #f #f)]))]))

(struct request (id method params) #:transparent)
(struct response (id) #:transparent)
(struct success-response response (result) #:transparent)
(struct error-response response (error) #:transparent)
(struct notification (method params) #:transparent)    

(define-machine server-init
  [(request id "initialize" params)
   (values (success-response id (hash))
           server-accept
           #f)])

(define-machine server-accept
  ['enter
   (values #f
           server-accept:read
           #f)])

(define-machine server-accept:read accept
  [(vector 'change text)
   #;
   (can
    #:transition some-fun
    #:reply ...
    #:delayed-reply ...
    )
   (values #f
           #f
           (delay/thread
            (notification "textDocument/publishDiagnostics"
                          (hash))))])

(struct machine-transition (state) #:transparent)
(struct machine-action (action) #:transparent)
(struct send-reply (message) #:transparent)


(define-syntax-rule (force-action exp)
  (let() 
    (define-values (_ _x p) exp)
    (force p)))

#;
(my comment yo

 
 (force-action (server-accept:read (vector 'change "stuff")))
 )