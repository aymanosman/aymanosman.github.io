#lang scribble/lp2

@(require (for-label racket/base
                     (only-in typed/racket/base : -> Values)))

@title{LSP --- Part 1}

@tabular[#:style 'boxed
         (list (list @bold{Topic:}   "The Language Server Protocol")
               (list @bold{Theme:}   "Introduction to the format of messages and the protocol")
               (list @bold{Audience:} "Racket users of an intermediate level and up"))]

@section{Messages}

The Language Server Protocol (LSP) defines three fundamental types of messages:
@italic{requests}, @emph{responses} and @emph{notifications}.

In LSP's
@hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15"]{documentation},
messages are described in the form of TypeScript interface declarations.
For example, here is the declaration for the base @italic{message} type.

@verbatim{
 interface Message {
  jsonrpc: string;
 }
}

In Racket this can be represented by a @racket[struct], as follows:

@chunk[<message-struct>
       (struct message (jsonrpc) #:transparent)]

It is on top of this structure that the three fundamental types of messages are defined.
For example, a @italic{request} looks like this:

@verbatim{
 interface RequestMessage extends Message {
  id: number | string;
  method: string;
  params?: array | object;
 }
}

@margin-note{Message types are documented
 @hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#base-protocol-json-structures"]{here}.
}

In Racket you can model this sort of relationship---where one data-type extends another---
using @racket[struct] directly.

@chunk[<request-struct>
       (struct request message (id method params) #:transparent)]

A @italic{response} has three fields, @italic{id}, @italic{result} and @italic{error},
but the last two are mutually exclusive.

@verbatim{
 interface ResponseMessage extends Message {
  id: number | string | null;
  result?: string | number | boolean | object | null;
  error?: ResponseError;
 }
}

Instead of modelling this as a single struct with optional fields,
I will model this as two separate variants of a common data type.

First, a generic response.

@chunk[<response-struct>
       (struct response message (id) #:transparent)]

And now the two variants.

@chunk[<success-and-error-response-structs>
       (struct success-response response (result) #:transparent)
       (struct error-response response (error) #:transparent)]

The only thing left to define is a @italic{notification},
which is simply a variant of the base message type.

@chunk[<notification-struct>
       (struct notification message (method params) #:transparent)]

@section{Protocol}

@margin-note{
 The protocol is documented
 @hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#actual-protocol"]{here}}

There are three distinct stages in the lifecycle of a language server.
@itemlist[
 #:style 'ordered
 @item{a "hand-shake" where client and server exchange information about each other}
 @item{a main loop where the server accepts,
  and responds to requests for language features (such as autocomplete)}
 @item{a shutdown stage where it prepares to terminate.}]

This suggests a state-machine of three states.
Which can be represented by three functions, one for each state.

@chunk[<server-machine>
       (define (server-init msg)
         <server-init-body>)
       
       (define (server-accept msg)
         <server-accept-body>)
       
       (define (server-shutdown msg)
         <server-shutdown-body>)]

A @italic{state-function} is a function which, given a @italic{message}, returns two values:
@itemlist[
 @item{a @italic{response}, to be sent to the client}
 @item{a @italic{state-function}, which indicates what state to transition to}]

Let's see how this works for the @italic{initialize} request.

@;subsection{The Initialize Request}

When the server is started, it awaits an
@hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#initialize"]{initialize}
request.

This is where the client declares its capabilities to the server
and vice versa.

@chunk[<server-init-body>
       (match msg
         [(request "2.0" id "initialize" params)
          (define result <server-capabilities>)
          (values (success-response "2.0" id result)
                  server-accept)]
         
         <server-init-error-cases>)]

@racket[server-init] responds only to @italic{initialize} requests,
it replies with some hard-coded value as server capabilities and transitions
to the @racket[server-accept] state.

The hard-coded value for server capabilities indicates to the client
that the server would like to receive the entire document on synchronisation events
and that the server provides the @italic{completion} feature.

@chunk[<server-capabilities>
       (hash 'capabilities
             (hash 'textDocumentSync TextDocumentSyncKind-Full
                   'completionProvider (hash)))]

Every other kind of message is treated as an error.

There are two types of error responses that the server makes.

@itemlist[
 @item{For a @italic{request} the @italic{reponse} should be an error with code -32002.}
 @item{@italic{Notifications} should be dropped, except for the @italic{exit notification},
  in which case, the server should exit abnormally with error-code 1}]

The payload of an @italic{error response} has the following definition.
When this is sent to the client,
it will be the value that is associated with the
@italic{error} key of the @italic{response message} 

@verbatim{
 interface ResponseError {
  code: number;
  message: string;
  data?: string | number | boolean | array | object | null;
 }
}

Here are the relavant cases:

@chunk[<server-init-error-cases>
       [(request "2.0" id _ _)
        (define err (hash 'code -32002
                          'message "bad request"))
        (values (error-response "2.0" id err)
                server-init)]
       
       [(notification "2.0" "exit" _)
        (exit 1)]
       
       [(notification "2.0" _ _)
        (values #f server-init)]]

The @italic{accept} state is where the server does most of its work.
It is also the state in which the server may be asked to shutdown.

@hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#shutdown"]{docs}

Shutting down the server is split into two stages:
1) a "shutdown" request, to which the server is to respond with "null" as its result.
and 2) an "exit" notification in which the server will actually terminate.

@chunk[<server-accept-body>
       (match msg
         <server-accept-main-cases>
         
         [(request "2.0" id "shutdown" params)
          (values (success-response "2.0" id (json-null))
                  server-shutdown)]
         
         [(notification "2.0" _ _)
          (values #f server-accept)])]

@chunk[<server-shutdown-body>
       (match msg
         [(notification "2.0" "exit" _)
          (exit 0)]
         
         [(notification "2.0" _ _)
          (values #f server-shutdown)])]

With the lifecycle messages out of the way,
we can focus on implementing actual langauge features,
for example @italic{completion}.

@hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_completion"]{docs}

@chunk[<server-accept-main-cases>
       [(request "2.0" id "textDocument/completion" params)
        (values (handle-completion id params) server-accept)]]

@chunk[<handle-completion>
       (define (handle-completion id params)
         (define result <completion-result>)
         (success-response "2.0" id result))]

You can imagine that in a real implementation that you would calculate a list
of completion candidates from the position of the cursor,
but here I just hard-code the result.

A successful response to this request can take multiple forms.
I chose @italic{CompletionList} because it is the most general.
A @italic{CompletionList} has the following shape:

@verbatim{
 interface CompletionList {
  isIncomplete: boolean;
  items: CompletionItem[];
 }
}

A @italic{CompletionItem} has many fields, all optionals, apart from one: @italic{label}.
Therefore, a complete value to represent a result looks like this.

@chunk[<completion-result>
       (hash 'isIncomplete #f
             'items (list (hash 'label "Racket"
                                'detail "... details")
                          (hash 'label "Emacs"
                                'detail "... details")
                          (hash 'label "module snippet"
                                'insertText "#lang racket

(module+ main
  (printf \"Hello Racket!\\n\"))\n")))]

It took a while to get here,
but this is the heart of the server,
the reason for it to exist.
Currently, there is only one feature supported,
but this is the part of the code that would expand to support more langauge features.

@section{Transport}

The server @italic{state-machine} is complete.
Now it is time to connect it to the client.

@margin-note{This is probably the trickiest (and least interesting part of the code).}

A Language Server typically communicates via
@italic{standard input} and @italic{ouput}.
Reading messages from the client on @italic{standard input}
and writing message to the client on @italic{standard output}.
Reading messages is handled by one function,
while writing is split into two.

@subsection{Writing Messages}

Sending a message consists of writing a stream of bytes to @italic{standard ouput}
in the following form.

@verbatim{
 Content-Length: ...\r\n
 \r\n
 {
  "jsonrpc": "2.0",
  "id": 1,
  "result": ...
 }
}

Much like @italic{HTTP}, what is written on the wire is split into two parts:
@italic{header} and @italic{content}.

There are only two kinds of header fields:
@italic{Content-Length} and @italic{Content-Type}.
Only @italic{Content-Length} is required since @italic{Content-Type} defaults to
@italic{application/vscode-jsonrpc; charset=utf-8}.

The @italic{content} is simply a json encoded value conforming to the message types described above.
The first thing to do is to translate Racket's structs into values suitable to pass to
Racket's json library.

@margin-note{See @racket[jsexpr?] for what counts as valid "json".}

@chunk[<send-message>
       (define (send-message msg [out (current-output-port)])
         (match msg
           [(success-response _ id result)
            (write-message/flush out (hash 'jsonrpc "2.0"
                                           'id id
                                           'result result))]
           
           [(error-response _ id error)
            (write-message/flush out (hash 'jsonrpc "2.0"
                                           'id id
                                           'error error))]))]

Now that we have our json shaped data, we can write some bytes to standard ouput.

@chunk[<write-message>
       (define (write-message/flush out h)
         (define content (jsexpr->string h))
         (fprintf out "Content-Length: ~a\r\n\r\n" (string-length content))
         (fprintf out "~a\n" content)
         (flush-output out))]

One thing that can catch you out is
the need for calling @racket[flush-output].
If you fail to flush output you may find that a Language Server Client will report calls timing out.
This is because the output has not been written, it is sitting in a buffer waiting to be flushed!
Forgetting to dutifully flush all output to either standard-output or standard-error was a frequent
source of frustrating bugs.

@subsection{Reading Messages}

Reading a message is broken up into two parts:
reading the @italic{header} and reading the @italic{content}.
The header can be discarded.
The spec defines only two kinds of header fields,
both are not very interesting to the functioning of the server.
The important thing to know for the purpose of parsing incoming messages
is that header fields are separated by @italic{carriage-return} and
@italic{linefeed} characters, i.e. @racket["\r\n"]
and that the @italic{header} and @italic{content} are separated
by a blank line, also ending in @racket["\r\n"].

Racket's @racket[read-line] accepts a @italic{mode} argument
which specifies the type of line ending to look for,
in this case @racket['return-linefeed].

An @racket[""] encountered signals the end of the @italic{header} part
and the start of the @italic{content} part.

@chunk[<read-header>
       (define (read-header in)
         (let loop ([fields '()])
           (match (read-line in 'return-linefeed)
             ["" (reverse fields)]
             [field (loop (cons field fields))])))]

The @italic{header} is ignored,
because it doesn't seem to play a part in the rest of the protocol.

@chunk[<read-message>
       (define (read-message [in (current-input-port)])
         (define _header (read-header in))
         <read-message-content>)]

The @italic{content} is @italic{JSON} formatted data.
For that we have @racket[read-json] of the json package.

@chunk[<read-message-content>
       (define content (read-json in))
       (match content
         [(hash-table ('jsonrpc "2.0")
                      ('method method)
                      ('params params))
          (if (hash-has-key? content 'id)
              (request "2.0"
                       (hash-ref content 'id)
                       method
                       params)
              (notification "2.0" method params))])]

All that was left to do was to distinguish between a
@italic{request} and a @italic{notification},
this was simply done by testing for the existence of the @italic{id} key
in the json object.

@section{Putting it all together}

@chunk[<*>
       <provides>
       
       (require racket/match
                json)

       <message-struct>
       <request-struct>
       <response-struct>
       <success-and-error-response-structs>
       <notification-struct>
       <misc>

       <server-machine>

       <handle-completion>

       <read-header>
       <read-message>
       <send-message>
       <write-message>

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

@section{Results}

This is what it looks like in Emacs:

@image["static/racket-lsp-emacs.png"]

And Intellij:

@image["static/racket-lsp-intellij.png"]

@section{Miscellaneous}

@margin-note{
 @tt{TextDocumentSyncKind} is documented
 @hyperlink["https://microsoft.github.io/language-server-protocol/specifications/specification-3-15/#textDocument_synchronization"]{here}
}

@chunk[<misc>
       (define TextDocumentSyncKind-Full 1)]

@chunk[<provides>
       (provide (struct-out message)
                (struct-out request)
                (struct-out response)
                (struct-out success-response)
                (struct-out error-response)
                (struct-out notification)
                server-init
                server-accept
                server-shutdown
                write-message/flush
                read-message
                main-loop)]

@section{Appendix: Logging}

@chunk[<log-message-io>
       (define (log-incoming-message msg)
         (match msg
           [(request _ id method _)
            (eprintf "Received request [id = ~s, method = ~s]\n" id method)]
           [(notification _ method _)
            (eprintf "Received notification [method = ~s]\n" method)])
         (flush-output (current-error-port)))

       (define (log-outgoing-message msg)
         (match msg
           [(success-response _ id _)
            (eprintf "Sent success response [id = ~s]\n" id)]
           [(error-response _ id _)
            (eprintf "Sent error response [id = ~s]\n" id)])
         (flush-output (current-error-port)))]

