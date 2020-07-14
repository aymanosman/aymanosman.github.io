#lang racket

(require syntax/parse)

(define mod-stx
  (parameterize ([read-accept-reader #t]
                 [port-count-lines-enabled #t]
                 [current-namespace (make-base-namespace)])
    (expand (read-syntax "part2.rkt" (open-input-file "part2.rkt")))))

;; mod-stx :: FullyExpanded

#;
(syntax-parse mod-stx
  #:literals (module #%plain-module-begin)
  [(module name:id path:expr
     (#%plain-module-begin
      configure-runtime-module:expr
      stuff:expr ...))
  
   ;; (println (list #'module #'name #'path))
   ;; (println #'configure-runtime-module)
   (define things (syntax->list #'(stuff ...)))
   (println (length things))
   (for ([s (take (drop things 65) 1)])
     (println s))])

(require syntax/kerncase)

#;
(kernel-syntax-case mod-stx
  [(module name:id path:expr
     (#%plain-module-begin
      configure-runtime-module:expr
      stuff:expr ...))
  
   ;; (println (list #'module #'name #'path))
   ;; (println #'configure-runtime-module)
   (define things (syntax->list #'(stuff ...)))
   (println (length things))
   #;
   (for ([s (take (drop things 65) 1)])
     (println s))
   ])