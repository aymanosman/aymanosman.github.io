#lang racket

(define (check-property p? gen)
  (define runs 10000)

  (define-values (failed? counter-example attempts)
    (let loop ([run 1])
      (cond
        [(= run (add1 runs))
         (values #f #f runs)]
        [else
         (define counter-example (gen))
         (cond
           [(p? counter-example)
            (loop (add1 run))]
           [else
            (values #t counter-example run)])])))

  (if failed?
      (printf "Found counter-example ~v, after ~a attempt(s)\n" counter-example attempts)
      (printf "Success, after running ~a test(s)\n" attempts)))

(define (int)
  (random 0 100000))

(check-property (lambda (n) (> n 500)) int)