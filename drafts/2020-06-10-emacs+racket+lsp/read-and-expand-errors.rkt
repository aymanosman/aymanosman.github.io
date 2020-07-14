#lang scribble/acmart


@(require scribble/manual)
@(require (for-label racket))

@(require scribble/example)

@title{A Quick Look at Read and Expand Errors}

@; - read may produce the @racket[eof-object?] @; Doesn't seam to cause problems

@bold{Read Errors}

@examples[
 #:label #f
 (eval:error (read-syntax "" (open-input-string "(foo")))]

We can capture this sort of exception.

@examples[
 #:label #f
 (with-handlers ([exn:fail:read?
                  (lambda (exn) exn)])
   (read-syntax "" (open-input-string "(foo")))]

The @racket[srcloc] returned is missing some data.

@racket[(struct src (source line column position span))]

But as long as we enable port line counting
we will have all the information we need.

@examples[
 #:label #f
 (with-handlers ([exn:fail:read?
                  (lambda (exn) exn)])
   (parameterize ([port-count-lines-enabled #t])
     (read-syntax "" (open-input-string "(foo"))))]

There is one more kind of error we can anticipate in the read phase

@examples[
 #:label #f
 (eval:error (parameterize ([read-accept-reader #t])
               (read-syntax "" (open-input-string "#lang foo"))))]

@examples[
 #:label #f
 (with-handlers ([exn:fail:filesystem?
                  (lambda (exn) exn)])
   (parameterize ([read-accept-reader #t]
                  [port-count-lines-enabled #t])
     (read-syntax "" (open-input-string "#lang foo"))))]

@bold{Expand Errors}

@examples[
 #:label #f
 (define stx
   (parameterize ([read-accept-reader #t])
     (read-syntax "" (open-input-string "#lang racket/base\nfoo"))))
 (eval:error (parameterize ([current-namespace (make-base-namespace)])
               (expand stx)))]

We can capture these errors too

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

In summary:

@itemlist[
 @item{read may raise @racket[exn:fail:read].}
 @item{read may raise @racket[exn:fail:filesystem] in the process of resolving module paths.}
 @item{expand may raise @racket[exn:fail:syntax].}
 ]
