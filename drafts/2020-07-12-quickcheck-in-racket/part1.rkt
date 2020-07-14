#lang scribble/lp2

@(require (for-label racket))

@chunk[<test>
       (define (int)
         (random 0 100000))

       (define (no-number-is-between-400-and-500 n)
         (not (and (<= 400 n)
                   (<= n 500))))

       (check-property int #:runs 10000 no-number-is-between-400-and-500)

       #;(check-property int #:runs 1000 number?)
       ]

@nested[#:style 'code-inset]{
 @racketoutput{
  Falsifiable, after 250 tests and 4 shrinks:
  @(linebreak)
  n = 400
}}

@chunk[<check-property>
       (define (check-property gen p? #:runs [runs 10000])
         (define-values (failed? counter-example tests) (falsify p? gen runs))

         (if failed?
             (let ()
               (define-values (shrunk shrinks) (shrink counter-example p?))    
               (printf "Falsifiable, after ~a tests and ~v shrinks:\nn = ~v\n" tests shrinks shrunk))
             (printf "OK, passed ~a tests\n" tests)))]

@racket[check-property] depends on two functions which are the core of minicheck:
@racket[falsify] and @racket[shrink].

The purpose of @racket[falsify] is to find a counter-example which shows that the property does not hold.

@chunk[<falsify>
       (define (falsify p? gen runs)
         (for/fold ([failed? #f]
                    [counter-example #f]
                    [tests 0])
                   ([run (in-range 1 (add1 runs))]
                    [example (in-producer gen)]
                    (code:comment "found counter-example")
                    #:final (not (p? example)))
           (values (not (p? example)) example run)))]

The purpose of @racket[shrink] is to find the @italic{smallest} counter-example.

@chunk[<shrink>
       (define (shrink counter-example p?)
         (code:comment "pick an arbitrary number to constrain how many times to attempt shrinking")
         (define max-shrinks 17)

         (for/fold ([shrunk counter-example]
                    [shrinks 0])
                   ([shrinks (in-range 1 (add1 max-shrinks))]
                    [shrunk (in-stream (shrink-stream p? 0 counter-example))])
           (values shrunk shrinks)))]

@racket[shrink-stream] will produce a stream of smaller counter-examples.

@chunk[<shrink-stream>
       (define (shrink-stream p? low counter-example)
         (define-values (new-low shrunk) (shrink-once p? low counter-example))
         (cond
           [shrunk
            (stream-cons shrunk
                         (shrink-stream p? new-low shrunk))]
           [else
            empty-stream]))]

@chunk[<shrink-once>
       (define (shrink-once p? low high)
         (for/fold ([low low]
                    [shrunk #f])
                   ([smaller (integer-stream low high)]
                    #:final (not (p? smaller)))
           (cond
             [(p? smaller)
              (code:comment "this smaller number is not a couner-example, make it the new lower-bound")
              (values smaller shrunk)]
             [else
              (code:comment "found smaller counter-example")
              (values low smaller)])))]

@chunk[<integer-stream>
       (define (integer-stream low high)
         (cond
           [(>= low high)
            empty-stream]
           [(= low (sub1 high))
            (stream-cons low empty-stream)]
           [else
            (let ([low_ (+ low (// (- high low) 2))])
              (stream-cons low (integer-stream low_ high)))]))

       (define (// z d)
         (floor (/ z d)))]

@chunk[<*>
       (require racket/stream)
       <check-property>
       <falsify>
       <shrink>
       <shrink-stream>
       <shrink-once>
       <integer-stream>
       <test>]
