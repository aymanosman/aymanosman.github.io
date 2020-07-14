#lang scribble/lp2

@(require (for-label racket/base))

@(require scribble/example)

@title{LSP -- Part 2}

@tabular[
 #:style 'boxed
 (list (list @bold{Topic:}   "The Language Server Protocol")
       (list @bold{Theme:}   "Implementing languages features for Racket")
       (list @bold{Audience:} "Racket users of an intermediate level and up"))]

Outline:

- Recap

- Diagnostics

- Read + Expand

- Next: drracket/check-syntax + Analysis

@section{Recap}

Welcome back to the series on The Language Server Protocol in Racket.

Last time we...

Recall we defined a state-machine with three states,
represented by three functions.

@racketblock[
 (define (server-init msg) ...)
 (define (server-accept msg) ...)
 (define (server-shutdown msg) ...)
 ]

Little will change with these.

@itemlist[
 @item{@italic{server-init} stays the same, except for a new value for server-capabilities.}
 @item{@italic{server-shutdown} stays the same}
 @item{@italic{server-accept} will be extended in the rest of this chapter to support the new capabilities}
 ]

Here is the new server capabilities.

@chunk[<server-capabilities>
       (hash 'capabilities
             (hash 'textDocumentSync TextDocumentSyncKind-Full))]

We actually just removed the @italic{completionProvider} capability!
This is because we will be focusing on so-called @italic{diagnostics}.
The server does not have to declare that it has the capability of
publishing diagnostics.

By focussing on diagnostics, I can postpone the dicussion of
handling requests that take the cursor position into account.
We will talk about that later.

Since a diagnostic is a one way message from server to client,
we need some way to trigger it.
The server can react to two notifications sent by the client.
We can react to two notifications that the client will send us:
@racket["textDocument/didChange"] and @racket["textDocument/didOpen"].

The new @italic{server-accept} will look like this:

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

Although each notification gets its own handler, @racket[handle-did-open] will
just call @racket[handle-did-change] for now.

The rest of this article is about how we implement @racket[handle-did-change].

@include-section["2-diagnostics.rkt"]

@section{The Implementation}

Last time we left the server we were in the middle of implementing the new
@racket[server-accept] function.

@racketblock[
 (define (server-accept msg)
   (match msg

     ...

     [(notification "textDocument/didOpen" params)
      (values (handle-did-open params)
              server-accept)]

     [(notification "textDocument/didChange" params)
      (values (handle-did-change params)
              server-accept)]

     ...))]

It is clear what we need to do next.
@italic{handle-did-change} has to return a @italic{notification} message containing the diagnostics
calculated over the file the client says has changed.

A did-change notification contains the following payload:

@verbatim{
 interface DidChangeTextDocumentParams {
  textDocument: TextDocumentIdentifier;
  contentChanges: TextDocumentContentChangeEvent[];
 }
}

@verbatim{
 interface TextDocumentIdentifier {
  uri: string;
 }
}

@verbatim{
 type TextDocumentContentChangeEvent = {
  text: string;
 }
}

So a complete value that we can expect from the client will look like this:

@racketblock[
 (hash 'textDocument (hash 'uri "file://some-file.rkt")
       'contentChanges (list (hash 'text "#lang racket\n... contents of file ...")))]

@racket[handle-did-change] extracts the @italic{document uri} and the @italic{document text}
from the payload of the notification.
Because we stipulated that we only support full document sync in @racket[TextDocumentSyncKind-Full]
we are given the contents of the entire file on every @racket["textDocument/didChange"] notification.
Calculating the actual diagnostics is delegated to another function.

@chunk[<handle-did-change>
       (define (handle-did-change params)
         (define uri (hash-ref (hash-ref params 'textDocument) 'uri))
         (define todo-path/uri (hash-ref (hash-ref params 'textDocument) 'uri))
         (define text (hash-ref (first (hash-ref params 'contentChanges)) 'text))

         (notification "textDocument/publishDiagnostics"
                       (hash 'uri uri
                             'diagnostics (get-diagnostics todo-path/uri text))))]

...

@chunk[<get-diagnostics>
       (define (get-diagnostics uri text #:todo-path-instead [a 123])
         (code:comment "(1) Install exception handlers")
         (with-handlers ([exn:fail:read? handle-read-error]
                         [exn:fail:filesystem? handle-filesystem-error]
                         [exn:fail:syntax? handle-syntax-error]
                         [exn:fail? handle-arbitrary-error])
           (parameterize ([read-accept-reader #t]
                          [port-count-lines-enabled #t]
                          <set-current-directory>
                          [current-namespace (make-base-namespace)])
             (code:comment "(2) do read + expansion")
             (expand (read-syntax (todo-path-relative-to-root uri) (open-input-string text))))
           (code:comment "(3) if no exceptions were thrown, we send an empty list of diagnostics")
           '()))]

@itemlist[
 #:style 'ordered
 @item{We install one exception handler for each kind of error we expect to encounter.}
 @item{We perform the read and expansions steps, throwing away the result.}
 @item{Finally, if no exception is thrown, we send the empty list of diagnostics.
  Because diagnostics are not accumulated on the client, sending an empty list of diagnostics
  has the effect of clearing the previously sent diagnostics.}
 ]

The @racket[current-directory] parameter is used in resolving module paths,
so we need to set it the parent directory of the file we are parsing.
Failing to do so will result in spurious filesystem errors.

@chunk[<set-current-directory>
       [current-directory (path->base (url->path (string->url uri)))]]

In setting the current directory we make use of a couple of auxilliary functions
that operate on @italic{uris} and @italic{paths}.

@margin-note{See the docs of @racket[split-path] for more info.}

@chunk[<operations-on-uri-and-path>
       (define (path->base p)
         (match/values (split-path p)
                       [(base _ #f) base]))

       (define (todo-path-relative-to-root uri)
         (match/values (split-path (url->path (string->url uri)))
                       [(_ name #f)
                        (build-path (current-root-path) name)]))]


@itemlist[
@;TODO: explain use of proper url->path
 @;item{@racket[uri->path] strips the @italic{scheme} part of the uri to yield a valid filesystem path.}
 @item{@racket[path->base] yields the directory part of a path (i.e. the file's parent directory.}
 @item{@racket[uri->path/relative-to-root] returns a relative path,
  starting at the root path of the workspace
  (as provided by the client in the initialize request).}]

Each exception handler knows how to convert a specific error into a diagnostic.
The @italic{severity} and @italic{message} components are straight-forward.
Calculating a @italic{range} depends on the lexical information attached to the exception.
The three cases are:
@itemlist[
 @item{No lexical information, in which case we yield a range that points to the beginning of the file.}
 @item{A @racket[srcloc] structure.}
 @item{A @racket[syntax] structure.}
 ]

@chunk[<handle-read-error>
       (define (handle-read-error exn)
         (list (hash 'severity DiagnosticSeverity-Error
                     'message (exn-message exn)
                     'range (exn:fail:read->range/jsexpr exn))))]

@chunk[<handle-filesystem-error>
       (define (handle-filesystem-error exn)
         (list (hash 'severity DiagnosticSeverity-Error
                     'message (exn-message exn)
                     'range beginning-of-file-range)))]

@chunk[<handle-syntax-error>
       (define (handle-syntax-error exn)
         (list (hash 'severity DiagnosticSeverity-Error
                     'message (exn-message exn)
                     'range (exn:fail:syntax->range/jsexpr exn))))]

@chunk[<handle-arbitrary-error>
       (define (handle-arbitrary-error exn)
         (list (hash 'severity DiagnosticSeverity-Error
                     'message (exn-message exn)
                     'range beginning-of-file-range)))]

@para{
 Interestingly, the function that extracts a @racket[srcloc] from the exception
 actually returns a (possibly empty) list of @racket[srcloc]s.

 If the list is empty,
 we will, again, just return a range pointing to the beginning of the file.

 Same goes for the function that extracts a @racket[syntax] from the exception.
}

@chunk[<extract-range-from-syntax-and-srcloc>
       (define beginning-of-file-range
         (hash 'start (hash 'line 0
                            'character 0)
               'end (hash 'line 0
                          'character 0)))
       
       (define (exn:fail:read->range/jsexpr exn)
         (match (exn:fail:read-srclocs exn)
           [(cons srcloc _)
            (srcloc->range/jsexpr srcloc)]
           [_ beginning-of-file-range]))

       (define (exn:fail:syntax->range/jsexpr exn)
         (match (exn:fail:syntax-exprs exn)
           [(cons stx _)
            (stx->range/jsexpr stx)]
           [_ beginning-of-file-range]))
       
       (define (srcloc->range/jsexpr s)
         (define line (sub1 (srcloc-line s)))
         (define column (srcloc-column s))
         (define span (srcloc-span s))
         (hash 'start (hash 'line line
                            'character column)
               'end (hash 'line line
                          'character (+ column span))))

       (define (stx->range/jsexpr stx)
         (define line (sub1 (syntax-line stx)))
         (define column (syntax-column stx))
         (define span (syntax-span stx))
         (hash 'start (hash 'line line
                            'character column)
               'end (hash 'line line
                          'character (+ column span))))]

@margin-note{So as long as the identifier we are refering to is only on one line (which is almost always the case),
 we can take this simple approach to calculating the range.}

The only noteworthy thing to add about the above code snippet
is that we have to subtract 1 from the result of calling @racket[syntax/srcloc-line],
that is because Racket counts lines using one-based indexing,
while the client expects zero-based indexing.

Finally, we can define @racket[handle-did-open] in terms of @racket[handle-did-change].

@chunk[<handle-did-open>
       (define (handle-did-open params)
         (define text
           (hash-ref (hash-ref params 'textDocument) 'text))
         (handle-did-change (hash-set params
                                      'contentChanges (list (hash 'text text)))))]

@section{Putting it all together}

A small change to the message structs.

We will remove the @italic{message} base structure,
as it only held a constant string "2.0".

@chunk[<message-structs>
       (struct request (id method params) #:transparent)
       (struct response (id) #:transparent)
       (struct success-response response (result) #:transparent)
       (struct error-response response (error) #:transparent)
       (struct notification (method params) #:transparent)]

@chunk[<*>
       <provides>

       (require racket/list
                racket/match
                (only-in net/url url->path
                                 string->url)
                json)

       <message-structs>

       <code-from-part1>

       <handle-did-change>
       <handle-did-open>
       <get-diagnostics>
       <handle-read-error>
       <handle-filesystem-error>
       <handle-syntax-error>
       <handle-arbitrary-error>
       <extract-range-from-syntax-and-srcloc>
       <operations-on-uri-and-path>

       (define current-root-path (make-parameter #f))

       (define DiagnosticSeverity-Error 1)

       (define (eprintf/flush . args)
         (apply eprintf args)
         (flush-output (current-error-port)))]

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
                current-root-path
                get-diagnostics
                write-message/flush
                read-message
                main-loop)]

@section{Appendix: Code from Part 1}

@chunk[<code-from-part1>
       (define TextDocumentSyncKind-Full 1)

       (define (server-init msg)
         (match msg
           [(request id "initialize" params)
            (current-root-path (url->path (string->url (hash-ref params 'rootUri))))
            (eprintf/flush "Set ROOT_PATH = ~v\n" (current-root-path))
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
            (values #f server-shutdown)]))

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

The only difference with chapter 1 is we now handle the new @italic{notification} message.

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
