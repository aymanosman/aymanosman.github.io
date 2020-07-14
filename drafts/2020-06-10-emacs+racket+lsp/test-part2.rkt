#lang racket

(require rackunit)

(require "./part2.rkt")

(define read-error-tests
  (test-suite
   "read errors"
   (test-case
    "unbalanced parentheses"
    (check-match
     (parameterize ([current-root-path "/"])
       (get-diagnostics "file:///dummy-file.rkt" "#lang racket (foo"))
     (list (hash-table ('severity 1)
                       ('message (? (lambda (s) (string-contains? s "read-syntax: expected a `)` to close `(`"))))
                       ('range (hash-table ('start (hash-table ('line 0)
                                                               ('character 13)))
                                           ('end (hash-table ('line 0)
                                                             ('character 14)))))))))

   (test-case
    "missing module"
    (check-match
     (parameterize ([current-root-path "/"])
       (get-diagnostics "file:///dummy-file.rkt" "#lang foo"))
     (list (hash-table ('severity 1)
                       ('message (? (lambda (s) (string-prefix? s "standard-module-name-resolver"))))
                       ('range (hash-table ('start (hash-table ('line 0)
                                                               ('character 0)))
                                           ('end (hash-table ('line 0)
                                                             ('character 0)))))))))))

(define expand-error-tests
  (test-suite
   "expand errors"
   (test-case
    "unbound identifier"
    (check-match
    (parameterize ([current-root-path "/"])
      (get-diagnostics "file:///dummy-file.rkt" "#lang racket foo"))
     (list (hash-table ('severity 1)
                       ('message (? (lambda (s) (string-contains? s "foo: unbound identifier"))))
                       ('range (hash-table ('start (hash-table ('line 0)
                                                               ('character 13)))
                                           ('end (hash-table ('line 0)
                                                             ('character 16)))))))))
   (test-case
    "no srclocs"
    (check-match
    (parameterize ([current-root-path "/"])
      (get-diagnostics "file:///dummy-file.rkt" "#lang scribble/lp2 "))
     (list (hash-table ('severity 1)
                       ('message (? (lambda (s) (string-contains? s "scribble/lp: no chunks"))))
                       ('range (hash-table ('start (hash-table ('line 0)
                                                               ('character 0)))
                                           ('end (hash-table ('line 0)
                                                             ('character 0)))))))))))

(define all-tests
  (test-suite
   "language-server"
   read-error-tests
   expand-error-tests))

(module+ test
  (require rackunit/text-ui)
  (run-tests all-tests))
