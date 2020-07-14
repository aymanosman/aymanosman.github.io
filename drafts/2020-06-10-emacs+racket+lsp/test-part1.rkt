#lang racket

(require rackunit
         json
         "part1.rkt")

(define protocol-tests
  (test-suite
   "protocol"
   (test-equal?
    "server-machine"
    (call-with-values (lambda ()
                        (server-init (request "2.0" 3 "initialize" #f)))
                      list)
    (list (success-response "2.0" 3 (hash 'capabilities (hash 'completionProvider (hash)
                                                              'textDocumentSync 1)))
          server-accept))))

(define transport-tests
  (test-suite
   "transport"
   (test-equal?
    "read-message"
    (let ()
      (define s (with-output-to-string
                  (lambda ()
                    (printf "Content-Length: ...\r\n\r\n")
                    (write-json (hash 'jsonrpc "2.0"
                                      'id 5
                                      'method "initialize"
                                      'params (hash 'something "here")))
                    (printf "Content-Length: ...\r\n\r\n")
                    (write-json (hash 'jsonrpc "2.0"
                                      'method "initialized"
                                      'params (json-null))))))
      (define in (open-input-string s))
      (define msg1 (read-message in))
      (define msg2 (read-message in))
      (list msg1 msg2))
    (list
     (request "2.0" 5 "initialize" (hasheq 'something "here"))
     (notification "2.0" "initialized" (json-null))))
   
   (test-equal?
    "read-message - longer header"
    (let ()
      (define s (with-output-to-string
                  (lambda ()
                    (printf "Content-Length: ...\r\n")
                    (printf "Content-Type: ...\r\n")
                    (printf "Some-Header: ...\r\n")
                    (printf "\r\n")
                    (write-json (hash 'jsonrpc "2.0"
                                      'id 5
                                      'method "initialize"
                                      'params (hash 'something "here")))
                    (printf "Content-Length: ...\r\n")
                    (printf "Content-Type: ...\r\n")
                    (printf "Some-Header: ...\r\n")
                    (printf "\r\n")
                    (write-json (hash 'jsonrpc "2.0"
                                      'method "initialized"
                                      'params (json-null))))))
      (define in (open-input-string s))
      (define msg1 (read-message in))
      (define msg2 (read-message in))
      (list msg1 msg2))
    (list
     (request "2.0" 5 "initialize" (hasheq 'something "here"))
     (notification "2.0" "initialized" (json-null))))))

(define all-tests
  (test-suite
   "language-server"
   protocol-tests
   transport-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
