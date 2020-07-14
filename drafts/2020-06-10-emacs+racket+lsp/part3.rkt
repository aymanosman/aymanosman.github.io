#lang scribble/lp2

- saving results of last analysis

- async and performance

- nested state-machine

- unused require diagnostic

- mutated variable diagnostic

@bold{the performance problem}

@racketblock[
 (define (handle-did-change params)
   ...

   (sleep 3)
         
   (notification "textDocument/publishDiagnostics"
                 (hash 'uri uri
                       'diagnostics empty)))]

@bold{the performance solution}

Be asynchronous.


NOTE: Intersting note
make-traverse namespace? [directory (current-directory)]? <-

@chunk[<*>
       42]
