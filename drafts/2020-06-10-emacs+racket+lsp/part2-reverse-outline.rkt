#lang scribble/manual

- async and performance

- more diagnostics

  + unused require diagnostic

  + mutated variable diagnostic

@bold{the performance problem}

- demonstrating the problem (sleep in handle-did-change)

- measuring performance

- async as a solution










@racketblock[
 (define (handle-did-change params)
   ...

   (sleep 3)

   (notification "textDocument/publishDiagnostics"
                 (hash 'uri uri
                       'diagnostics empty)))]

the emacs implementation of lsp actually handles this gracefully

intellij-lsp, on the other hand, becomes completely unresponsive, and even crashes the IDE.


