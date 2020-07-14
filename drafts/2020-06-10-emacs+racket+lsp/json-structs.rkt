#lang racket

(require (for-syntax racket/list
                     syntax/parse))

(begin-for-syntax
  (define (check-fields names kws stx)
    (define table (make-hash))
    ;; Only permitted names
    (for ([n (in-list kws)])
      (define kw (syntax-e n))
      (unless (member kw names)
        (raise-syntax-error #f "expected one of #:start #:end" n))
      (hash-update! table kw (lambda (count)
                               (cond
                                 [(= 0 count)
                                  1]
                                 [else
                                  (raise-syntax-error #f "duplicated field name" n)])) 0))
    ;; Required to appear
    (define missing '())
    (for ([kw (in-list names)])
      (unless (hash-has-key? table kw)
        (set! missing (cons kw missing))))
    (cond
      [(empty? missing)
       (void)]
      [(= 1 (length missing))
       (raise-syntax-error #f (format "missing field: ~v" (first missing)) stx)]
      [else
       (raise-syntax-error #f (format "missing fields: ~v" missing) stx)]))

  (define (keyword-syntax->symbol k)
    (string->symbol (keyword->string (syntax-e k)))))


(define-for-syntax (Range-fn stx)
  (define names
    (list '#:start '#:end))
  (syntax-parse stx
    [(_ (~seq kw:keyword arg:expr) ...)
     #:with (x ...) (map keyword-syntax->symbol (syntax->list #'(kw ...)))
     (check-fields names (syntax->list #'(kw ...)) stx)
     #'(hash (~@ 'x arg) ...)]))

(define-syntax Range Range-fn)

(Range #:end 1 #:start 2)
