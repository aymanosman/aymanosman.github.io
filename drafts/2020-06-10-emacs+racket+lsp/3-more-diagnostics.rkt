#lang scribble/lp2

@subsection{More Diagnostic}

Explain: expansion and unbound identifier problem

@chunk[<analyser-unbound-identifier>
       (define unbound-identifiers '())

       (define/public (unbound-identifier stx str)
         (set! unbound-identifiers
               (cons (hash 'message str
                           'range (stx->range/jsexpr stx))
                     unbound-identifiers)))

       (define/public (get-unbound-identifiers)
         unbound-identifiers)]

@chunk[<analyser-syntax-error>
       (define syntax-errors '())

       (define/public (syntax-error stx str)
         (set! syntax-errors
               (cons (hash 'message str
                           'range (stx->range/jsexpr stx))
                     syntax-errors)))

       (define/public (get-syntax-errors)
         syntax-errors)]

@chunk[<analyser-unused-identifiers>
       (define unused-identifiers '())

       (define/override (syncheck:add-text-type stx
                                                start
                                                end
                                                text-type)
         (println stx)
         (println text-type)
         
         (case text-type
           [(unused-identifier)
            (set! unused-identifiers
                  (cons (hash 'message "unused identifier"
                              'tags (list 1)
                              'severity 2
                              'range (stx->range/jsexpr stx))
                        unused-identifiers))]))

       (define/public (get-unused-identifiers)
         unused-identifiers)]