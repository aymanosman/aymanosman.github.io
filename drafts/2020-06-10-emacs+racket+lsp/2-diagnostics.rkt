#lang scribble/manual

@(require (for-label racket))

@(require scribble/example)

@title{Diagnostics}

The final result.

@image["static/example-diagnostics.png"]

Diagnostics will allow us to inform the client of certain kinds of validations
the server can perform on a given file.
For example, marking imports and variables as unused,
or certain identifiers as unbound.
The screenshot above shows one such diagnostic, which the IDE displays
as a red squiggly line under the offending variable.

But first, what do the diagnostics that we send to the client look like?

@bold{Sending Diagnostics}

Diagnostics are sent from the server to the client in a
notification with method @racket["textDocument/publishDiagnostics"].

The payload has the following shape:

@verbatim{
 interface PublishDiagnosticsParams {
  uri:         string;
  diagnostics: Diagnostic[];
 }
}

While a Diagnostic itself has the following shape:

@verbatim{
 export interface Diagnostic {
  range:     Range;
  severity?: DiagnosticSeverity;
  message:   string;
 }
}

DiagnosticSeverity can be one of Error, Warning, Information or Hint,
assigned the numbers 1 through 4.

A Range is a value that describes a span of text.

@verbatim{
 interface Position {
  line:      number;
  character: number;
 }
          
 interface Range {
  start: Position;
  end:   Position;
 }               
}

So a complete diagnostics notification looks like this:

@racketblock[
 (hash 'uri "file:///some-file.rkt"
       'diagnostics (list (hash 'severity 1
                                'message "something bad here"
                                'range (hash 'start (hash 'line 3
                                                          'character 9)
                                             'end (hash 'line 3
                                                        'character 12)))))]

Now that we know what diagnostics look like,
let us turn to how to calculate them.

@bold{Calculating Diagnostics}

Any analysis in Racket will start by reading the text of the program being analysed.

@examples[
 #:label #f
 (define in (open-input-string "(require json racket/list)"))
 (port-count-lines! in)
 (define stx (read-syntax "dummy-file.rkt" in))
 stx
 (define json-stx (list-ref (syntax-e stx) 1))
 json-stx
 (syntax-line json-stx)
 (syntax-column json-stx)
 (syntax-span json-stx)]

Reading produces a so-called @italic{syntax object},
a structure that holds a representation of the source code
that was read along with information that links it
back to the text it came from (such as line and column numbers).

Racket has an additional phase called @italic{expansion},
where higher level constructs are transformed into lower level ones.
We are interested in these @italic{fully expanded} programs
because they are inputs to later analysis stages that we will eventually do.
Moreover, @italic{expansion} is interesting in itself because it catches certain kinds of syntax errors.

@examples[
 #:label #f
 (parameterize ([port-count-lines-enabled #t]
                [current-namespace (make-base-namespace)])
   (expand
    (read-syntax "dummy-file.rkt"
                 (open-input-string "(require json racket/list)"))))]

I have already alluded to the fact that reading and expansion can go wrong.
For example, reading could encounter unbalanced parentheses.
An exception will be raised and reading stops.
Expansion, similarly can fail.
These failures will form the basis of our first diagnostics.
We will get to more sophisticated diagnostics in a later article.

@bold{Read Errors}

The first kind of error we can anticipate in the read phase
is what you would expect to be called a "syntax" error in other systems.

@examples[
 #:label #f
 (eval:error (read-syntax "" (open-input-string "(foo")))]

We can capture this sort of exception using @racket[exn:fail:read?],
and as long as we enable line counting on ports,
we will have the information we need to construct a valid @italic{range} value.

@examples[
 #:label #f
 (with-handlers ([exn:fail:read?
                  (lambda (exn) exn)])
   (parameterize ([port-count-lines-enabled #t])
     (read-syntax "" (open-input-string "(foo"))))]

The other kind of error we can anticipate in the read phase is an error in resolving modules.

@examples[
 #:label #f
 (eval:error (parameterize ([read-accept-reader #t])
               (read-syntax "" (open-input-string "#lang foo"))))]

These can be caught by @racket[exn:fail:filesystem?].

@examples[
 #:label #f
 (with-handlers ([exn:fail:filesystem?
                  (lambda (exn) exn)])
   (parameterize ([read-accept-reader #t]
                  [port-count-lines-enabled #t])
     (read-syntax "" (open-input-string "#lang foo"))))]

@bold{Expand Errors}

Expansion can fail due to unbound identifiers.

@examples[
 #:label #f
 (define stx
   (parameterize ([read-accept-reader #t])
     (read-syntax "" (open-input-string "#lang racket/base\nfoo"))))
 (eval:error (parameterize ([current-namespace (make-base-namespace)])
               (expand stx)))]

We can capture these errors using @racket[exn:fail:syntax?].

@examples[
 #:label #f
 (define stx
   (parameterize ([read-accept-reader #t]
                  [port-count-lines-enabled #t])
     (read-syntax "" (open-input-string "#lang racket/base\nfoo"))))
 (with-handlers ([exn:fail:syntax?
                  (lambda (exn) exn)])
   (parameterize ([current-namespace (make-base-namespace)])
     (expand stx)))]

Finally, expansion can fail due to arbitrary errors.
Any exception may be raised by a syntactic form ---
they are arbitrary Racket code, after all.
Therefore we have a catch-all case in the end to handle that.
Note that,
because we do not know the nature of the error
we cannot extract source location information
and have to rely solely on the message of the exception.

The contents of the file in the example below looks like this:

@filebox["code.rkt"]{
@codeblock{
#lang racket/base

(require (for-syntax racket/base))

(define-syntax (bad-macro _)
  (raise (exn:fail "bork" (current-continuation-marks))))

(bad-macro ...)
}
}

@examples[
 #:label #f
 (parameterize ([read-accept-reader #t]
                [port-count-lines-enabled #t]
                [current-namespace (make-base-namespace)])
   (with-handlers ([exn:fail? (lambda (exn) exn)])
     (expand
      (read-syntax "" (open-input-string #<<EOF
#lang racket/base

(require (for-syntax racket/base))

(define-syntax (bad-macro _)
  (raise (exn:fail "bork" (current-continuation-marks))))

(bad-macro ...)
EOF
                                         )))))
 ]

In summary:

@itemlist[
 @item{read may raise @racket[exn:fail:read].}
 @item{read may raise @racket[exn:fail:filesystem] in the process of resolving module paths.}
 @item{expand may raise @racket[exn:fail:syntax].}
 @item{expand may raise an arbitrary @racket[exn:fail].}
 ]

We now have all the pieces we need to implement the new parts of our server.

