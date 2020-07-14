#lang racket

(define (read-module path)
  (parameterize ([read-accept-reader #t]
                 [port-count-lines-enabled #t])
    (with-handlers ([exn:fail:read?
                     (lambda (exn) exn)])
      (read-syntax path (open-input-file path)))))

(define base-namespace (make-base-namespace))

(define (expand-module stx)
  (parameterize ([current-namespace base-namespace])
    (with-handlers ([exn:fail:syntax?
                     (lambda (exn) exn)])
      (expand stx))))

;; READ + EXPAND

(define mod-stx
  (match (read-module "/tmp/test.rkt")
    [(? syntax? stx)
     (match (expand-module stx)
       [(? syntax? stx)
        stx]
       [(? exn:fail:syntax? exn)
        exn])]
  
    [(? exn:fail:read? exn)
     exn]))

;; ANALYSE

(require drracket/check-syntax)

(define analyser%
  (class (annotations-mixin object%)
    (super-new)
                    
    (define/override (syncheck:find-source-object stx)
      stx)

    (define/override (syncheck:add-unused-require stx start end)
      (add-annotation (vector 'unused-require stx)))
                    
    (define annotations '())

    (define/private (add-annotation a)
      (set! annotations (cons a annotations)))

    (define/public (get-annotations)
      annotations)))

(define (analyse stx)
  (define ana (new analyser%))

  (define-values (supply-syntax done) (make-traversal base-namespace #f))

  (parameterize ([current-annotations ana])
    (supply-syntax stx)
    (done))

  (send ana get-annotations))


(analyse mod-stx)


