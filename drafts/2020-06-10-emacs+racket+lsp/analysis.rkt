#lang scribble/lp2


@section{Analysis}

... much more to be said before we get to analyse (read + expand errors) ...

@chunk[<analyse>
       (define (analyse text)
         (define ana (new analyser%))

         (with-handlers ([exn:fail:read?
                          (lambda (exn)
                            (send ana read-error exn)
                            (values ana #t))])

           (define unexpanded-form
             (parameterize ([read-accept-reader #t]
                            [port-count-lines-enabled #t])
               (read-syntax "" (open-input-string text))))

           ;; TODO If read succeeds, really should clear previous read-error diagnostics

           (define failed? (run-traversal unexpanded-form ana))

           (values ana failed?)))]



@chunk[<analyser%>
       (define analyser%
         (class (annotations-mixin object%)
           (super-new)

           (define/override (syncheck:find-source-object stx)
             stx)

           ;; read
           ;; <analyser-read-error>

           ;; expand
           ;; <analyser-unbound-identifier>
           ;; <analyser-syntax-error>

           ;; analyse
           ;; <analyser-unused-require>
           ;; <analyser-unused-identifiers>

           ))]

You extract information from the annotations object
by overriding methods which name the type of annotation
it was able to create.
This will serve as our diagnostic information.

We need to override @racket[syncheck:add-unused-require].

@(define add-unused-require-url "https://docs.racket-lang.org/drracket-tools/Accessing_Check_Syntax_Programmatically.html?q=syncheck#%28meth._%28%28%28lib._drracket%2Fcheck-syntax..rkt%29._syncheck-annotations~3c~25~3e%29._syncheck~3aadd-unused-require%29%29")

@margin-note{The documentation for this method is @hyperlink[add-unused-require-url]{here}.}

This method received three arguments,
@italic{stx} which is the syntax-object of the unused require,
and the span of text it occupies represents as a pair of integers @italic{left} and @italic{right}.

We can choose to store this information however we want
but it may be useful to look ahead at how we will end up
communicating this to the client in order to decide
the best form to store this in.

@chunk[<analyser-unused-require>
       (define unused-requires '())

       (define/override (syncheck:add-unused-require stx
                                                     left
                                                     right)
         (set! unused-requires
               (cons (stx->range/jsexpr stx) unused-requires)))

       (define/public (get-unused-requires)
         unused-requires)]

We must configure @racket[read-syntax] to accept @italic{#lang} in order
to read the contents of our file.
This is done via the parameter @racket[read-accept-reader].

TODO: Error Handling

- read-syntax can fail

- expand can fail


@chunk[<analyser-read-error>
       (define read-errors '())
       (define/public (read-error exn)
         (define errs
           (map (lambda (s)
                  (hash 'severity 1
                        'message (exn-message exn)
                        'range (srcloc->range/jsexpr s)))
                (exn:fail:read-srclocs exn)))

         (set! read-errors
               (append errs read-errors)))
       (define/public (get-read-errors)
         read-errors)]

@chunk[<run-traversal>
       (define (run-traversal stx ann)
         (define base-namespace (make-base-namespace))

         (define-values (supply-syntax done)
           (make-traversal base-namespace #f))

         (parameterize ([current-annotations ann]
                        [current-namespace base-namespace])
           (define failed? #f)
           (with-handlers ([exn:fail:syntax:unbound?
                            (lambda (exn)
                              (set! failed? #t)
                              (send ann unbound-identifier (first (exn:fail:syntax-exprs exn))
                                    (exn-message exn)))]
                           [exn:fail:syntax?
                            (lambda (exn)
                              (set! failed? #t)
                              (send ann syntax-error (first (exn:fail:syntax-exprs exn))
                                    (exn-message exn)))])
             (supply-syntax (expand stx)))
           (done)
           failed?))]

@verbatim{
 interface DidChangeTextDocumentParams {
  textDocument: VersionedTextDocumentIdentifier;
  contentChanges: TextDocumentContentChangeEvent[];
 }
}

@verbatim{
 export type TextDocumentContentChangeEvent = {
  text: string;
 }
}
