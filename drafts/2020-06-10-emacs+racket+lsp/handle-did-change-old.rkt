#lang scribble/lp2

@chunk[<handle-did-change>
       (define last-analysis #f)
       
       (define (handle-did-change params)
         (define uri (hash-ref (hash-ref params 'textDocument) 'uri))

         (define text (~> (hash-ref params 'contentChanges)
                          (first)
                          (hash-ref 'text)))

         (define-values (ana failed?) (analyse text))

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
                                           #;
                                           (analysis-unused-identifiers (if (and failed? last-analysis)
                                                                            last-analysis
                                                                            ana))))))]

@chunk[<handle-did-open>
       (define (handle-did-open params)
         (define text
           (hash-ref (hash-ref params 'textDocument) 'text))
         (handle-did-change (hash-set params
                                      'contentChanges (list (hash 'text text)))))]