#lang scribble/lp2

@(require (for-label racket/base
                     drracket/check-syntax))

@title{LSP -- Part 2}

@tabular[#:style 'boxed
         (list (list @bold{Topic:}   "The Language Server Protocol")
               ;; drracket/check-syntax of drracket-tools-lib can be used to ...
               (list @bold{Theme:}   "Implementing languages features for Racket")
               (list @bold{Audience:} "Racket users of an intermediate level and up"))]

Last part we looked at ...

@(define (comment . args) "")

@comment{

 hover -> bound occurences??? type sig? documentation?
 documentHighlight -> highlight bound accurences
 references ->
 definition -> go to def
 rename
 completion

 diagnostics
 - unused identifiers
 - unused requires
}

@section{Recap}

This is the previsously defined structs with message shared struct taken out.

@chunk[<message-structs>
       (struct request (id method params) #:transparent)
       (struct response (id) #:transparent)
       (struct success-response response (result) #:transparent)
       (struct error-response response (error) #:transparent)
       (struct notification (method params) #:transparent)]

This the previously defined state-machine.

server-init stays the same except we are going to define new server capabilities

server-shutdown stays the same

server-accept will be extended in the rest of this chapter to support the new capabilities

@chunk[<server-machine>
       (define (server-init msg)
         (match msg
           [(request id "initialize" params)
            (define result <server-capabilities>)
            (values (success-response id result)
                    server-accept)]

           [(request id _ _)
            (define err (hash 'code -32002
                              'message "bad request"))
            (values (error-response id err)
                    server-init)]

           [(notification "exit" _)
            (exit 1)]

           [(notification _ _)
            (values #f server-init)]))

       <server-accept>

       (define (server-shutdown msg)
         (match msg
           [(notification "exit" _)
            (exit 0)]

           [(notification _ _)
            (values #f server-shutdown)]))]

Here is the new server capabilities.

@chunk[<server-capabilities>
       (hash 'capabilities
             (hash 'textDocumentSync TextDocumentSyncKind-Full))]

We actually just removed the completionProvider capability!
This is because we will be focusing on so-called "diagnostics"
which the server does not have to declare as a capability.

The reason to focus on diagnostics is we don't have to take the
cursor position into account, and I want to postpone that discussion
till later.

Since a diagnostic is a one way message from server to client,
we need some way to trigger it.
One possibility is to react to the "textDocument/didChange"
and "textDocument/didOpen" notificaitions from the client.
That is the approach I take.

Therefore server-accept will now look like this:

@chunk[<server-accept>
       (define (server-accept msg)
         (match msg
           [(request id "shutdown" params)
            (values (success-response id (json-null))
                    server-shutdown)]

           [(notification "textDocument/didOpen" params)
            (values (handle-did-open params)
                    server-accept)]

           [(notification "textDocument/didChange" params)
            (values (handle-did-change params)
                    server-accept)]

           [(notification _ _)
            (values #f server-accept)]))]

In both cases, we will calculate the same diagnostics.

We define @italic{handle-did-change} in the next section.

@section{Diagnostics}

We will start with a simple diagnostic:
marking unused requires.

To see how this can be done, an introduction to the drracket/check-syntax module is in order.

... drracket/check-syntax stuff ...

@(define mixin-url "https://docs.racket-lang.org/guide/classes.html#%28part._.Mixins%29")

The functionality of drracket/check-syntax is exposed by
the @racket[annotation-mixin] @hyperlink[mixin-url]{mixin}.

@chunk[<analyser%>
       (define analyser%
         (class (annotations-mixin object%)
           (super-new)

           (define/override (syncheck:find-source-object stx)
             stx)

           ;; read
           <analyser-read-error>

           ;; expand
           <analyser-unbound-identifier>
           <analyser-syntax-error>

           ;; analyse
           <analyser-unused-require>
           <analyser-unused-identifiers>))]

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

@subsection{Sending Diagnostics}

Diagnostics are sent from the server to the client in a
notification with method "textDocument/publishDiagnostics".

The payload has the following shape:

@verbatim{
 interface PublishDiagnosticsParams {
  uri: DocumentUri;
  diagnostics: Diagnostic[];
 }
}

While a Diagnostic itself has the following shhape:

@verbatim{
 export interface Diagnostic {
  range: Range;
  severity?: DiagnosticSeverity;
  message: string;
 }
}

DiagnosticSeverity can be one of Error, Warnging, Information or Hint.

a Range will prove the most difficult to calculate

A Range is value that describes the span of text that we wish to annotate.

@verbatim{
 interface Position {
  line: number;
  character: number;
 }

 interface Range {
  start: Position;
  end: Position;
 }
}

@racketblock[
 (hash 'uri "file:///some-file.rkt"
       'diagnostics (list (hash 'severity 1
                                'message "something bad here"
                                'range (hash 'start (hash 'line 3
                                                          'characeter 9)
                                             'end (hash 'line 3
                                                        'character 12)))))]

@italic{syntax objects} have enough information in them
for us to come up with a valid Range to send to the client.

@(require scribble/example)

@examples[
 (define in (open-input-string "(require json racket/list)"))
 (port-count-lines! in)
 (define stx (read-syntax "dummy-file.rkt" in))
 stx
 (define json-stx (list-ref (syntax-e stx) 1))
 json-stx
 (syntax-line json-stx)
 (syntax-column json-stx)
 (syntax-span json-stx)]

As you can see in the example above,
we made sure to call @racket[port-count-lines!] so that the resulting syntax object has line number information.

So as long as the identifier we are refering to is only on one line (which is almost always the case)
we can take this simple approach.

@chunk[<stx->range/jsexpr>
       (define (stx->range/jsexpr stx)
         (define line (sub1 (syntax-line stx)))
         (define column (syntax-column stx))
         (define span (syntax-span stx))
         (hash 'start (hash 'line line
                            'character column)
               'end (hash 'line line
                          'character (+ column span))))]

For now, I am skipping defining a racket struct to represent a range
and going straight to a jsexpr.
The only noteworthy thing to add to the above code snippet
is that we have to covert to one-based indexing of lines that Racket produces
to the zero-based indexing that client will expect.

Now that we know what we are going to send the diagnostics,
let us turn to how we are going to calculate them.

@subsection{Calculating Diagnostics}

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

@chunk[<analyse>
       (define (analyse text #:uri [uri "dummy-file.rkt"])
         (define in (open-input-string text))

         (port-count-lines! in)

         (define ana (new analyser%))

         (with-handlers ([exn:fail:read?
                          (lambda (exn)
                            (send ana read-error exn)
                            (values ana #t))])

           (define unexpanded-form
             (parameterize ([read-accept-reader #t])
               (read-syntax uri in)))

           ;; TODO If read succeeds, really should clear previous read-error diagnostics

           (define failed? (run-traversal unexpanded-form ana))

           (values ana failed?)))

       <run-traversal>]

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

@chunk[<srcloc->range/jsexpr>
       (define (srcloc->range/jsexpr s)
         (define line (sub1 (srcloc-line s)))
         (define col (srcloc-column s))
         (define span (srcloc-span s))
         (hash 'start (hash 'line line
                            'character col)
               'end (hash 'line line
                          'character (+ col span))))]

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

@chunk[<handle-did-change>
       (define last-analysis #f)

       (define (handle-did-change params)
         (define uri (hash-ref (hash-ref params 'textDocument) 'uri))

         (define text (~> (hash-ref params 'contentChanges)
                          (first)
                          (hash-ref 'text)))

         (define-values (ana failed?) (analyse text #:uri uri))

         (when (not failed?)
           (set! last-analysis ana))

         ;; expand

         (define unbound-identifiers-diagnostics
           (map (lambda (h)
                  (hash 'range (hash-ref h 'range)
                        'message (hash-ref h 'message)
                        'severity 1))
                (analysis-unbound-identifiers ana)))

         (define generic-syntax-errors-diagnostics
           (map (lambda (h)
                  (hash 'range (hash-ref h 'range)
                        'message (hash-ref h 'message)
                        'severity 1))
                (analysis-generic-syntax-errors ana)))

         ;; analyse

         (define unused-requires-diagnostics
           (map (lambda (r)
                  (hash 'range r
                        'severity 2
                        'message "unused require"))
                (analysis-unused-requires (if (and failed? last-analysis)
                                              last-analysis
                                              ana))))

         (notification "textDocument/publishDiagnostics"
                       (hash 'uri uri
                             'diagnostics (append
                                           ;; read
                                           (analysis-read-errors ana)
                                           ;; expand
                                           unbound-identifiers-diagnostics
                                           generic-syntax-errors-diagnostics
                                           ;; analyse
                                           unused-requires-diagnostics
                                           (analysis-unused-identifiers (if (and failed? last-analysis)
                                                                            last-analysis
                                                                            ana))))))]

@chunk[<handle-did-open>
       (define (handle-did-open params)
         (define text
           (hash-ref (hash-ref params 'textDocument) 'text))
         (handle-did-change (hash-set params
                                      'contentChanges (list (hash 'text text)))))]


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

@chunk[<*>
       <provides>

       (require racket/class
                racket/list
                racket/match
                drracket/check-syntax
                json
                threading)

       <message-structs>

       <part1>

       <analyser%>
       <analyse>

       <stx->range/jsexpr>
       <srcloc->range/jsexpr>

       ;; <analysis>
       (define (analysis-read-errors ana)
         (send ana get-read-errors))

       (define (analysis-unused-requires ana)
         (send ana get-unused-requires))

       (define (analysis-unbound-identifiers ana)
         (send ana get-unbound-identifiers))

       (define (analysis-generic-syntax-errors ana)
         (send ana get-syntax-errors))

       (define (analysis-unused-identifiers ana)
         (send ana get-unused-identifiers))

       (define (eprintf/flush . args)
         (apply eprintf args)
         (flush-output (current-error-port)))


       <handle-did-change>
       <handle-did-open>

       ]

@section{Miscellaneous}

@chunk[<provides>
       (provide (struct-out request)
                (struct-out response)
                (struct-out success-response)
                (struct-out error-response)
                (struct-out notification)
                server-init
                server-accept
                server-shutdown
                write-message/flush
                read-message
                analyse
                analysis-read-errors
                analysis-unbound-identifiers
                analysis-generic-syntax-errors
                analysis-unused-identifiers
                main-loop)]

@section{Appendix: Code from Part 1}

@chunk[<part1>
       (define TextDocumentSyncKind-Full 1)

       <server-machine>

       <read-and-write-messages>

       <log-message-io>

       (define (main-loop [server server-init])
         (eprintf "Server state ~v\n" (object-name server))
         (flush-output (current-error-port))

         (define msg (read-message))

         (log-incoming-message msg)

         (define-values (response next-server) (server msg))

         (when response
           (send-message response)
           (log-outgoing-message response))

         (main-loop next-server))]

@section{Appendix: Transport}

The only difference with chapter 1 is we add the ability to send a notification

@chunk[<send-message-notification>
       [(notification method params)
        (write-message/flush out (hash 'jsonrpc "2.0"
                                       'method  method
                                       'params params))]]

@chunk[<read-and-write-messages>
       (define (read-header in)
         (let loop ([fields '()])
           (match (read-line in 'return-linefeed)
             ["" (reverse fields)]
             [field (loop (cons field fields))])))

       (define (read-message [in (current-input-port)])
         (define _header (read-header in))
         (define content (read-json in))
         (match content
           [(hash-table ('jsonrpc "2.0")
                        ('method method)
                        ('params params))
            (if (hash-has-key? content 'id)
                (request (hash-ref content 'id)
                         method
                         params)
                (notification method params))]))

       (define (send-message msg [out (current-output-port)])
         (match msg
           [(success-response id result)
            (write-message/flush out (hash 'jsonrpc "2.0"
                                           'id id
                                           'result result))]

           [(error-response id error)
            (write-message/flush out (hash 'jsonrpc "2.0"
                                           'id id
                                           'error error))]
           <send-message-notification>))

       (define (write-message/flush out h)
         (define content (jsexpr->string h))
         (fprintf out "Content-Length: ~a\r\n\r\n" (string-length content))
         (fprintf out "~a\n" content)
         (flush-output out))]


@section{Appendix: Logging}

@chunk[<log-message-io>
       (define (log-incoming-message msg)
         (match msg
           [(request id method _)
            (eprintf "Received request [id = ~s, method = ~s]\n" id method)]
           [(notification method _)
            (eprintf "Received notification [method = ~s]\n" method)])
         (flush-output (current-error-port)))

       (define (log-outgoing-message msg)
         (match msg
           [(success-response id _)
            (eprintf "Sent success response [id = ~s]\n" id)]
           [(error-response id _)
            (eprintf "Sent error response [id = ~s]\n" id)]
           [(notification method _)
            (eprintf "Sent notification [method = ~a]\n" method)])
         (flush-output (current-error-port)))]
