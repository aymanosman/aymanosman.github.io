#lang scribble/manual

@(require (for-label racket
                     drracket/check-syntax))

@(require scribble/example)

@(require racket/contract)

@title{A Tour of drracket/check-syntax}

Given a file with the following contents:

@racketmod[
 #:file "test.rkt"
 racket/base
 
 (require json)]

We can use the function @racket[show-content] to get a preview of the sort of analysis
we can gain access to.

@examples[
 (require drracket/check-syntax)
 (define stx
   (parameterize ([read-accept-reader #t] (code:comment "(1)")
                                          [port-count-lines-enabled #t]) (code:comment "(2")
     (read-syntax "test.rkt" (open-input-string "#lang racket/base\n\n(require json)"))))
 (show-content stx)]

@itemlist[
 #:style 'ordered
 @item{This parameter makes @racket[read-syntax] accept "#lang"}
 @item{This parameter means the resulting syntax-object will contain
  the line and column information we need to calculate range values to the client}]

@racket[show-content] returns a list of vectors which represent annotations.

@racketresultblock[
 '(#(syncheck:add-require-open-menu ...)
   #(syncheck:add-require-open-menu ...)
   #(syncheck:add-text-type ... document-identifier)
   #(syncheck:add-docs-menu ... require ...)
   #(syncheck:add-jump-to-definition ... require ...)
   #(syncheck:add-mouse-over-status ... "imported from “racket/base”")
   #(syncheck:add-arrow/name-dup/pxpy ...)
   #(syncheck:add-unused-require ...)
   #(syncheck:add-text-type ... unused-identifier)
   #(syncheck:add-mouse-over-status ... "1 bound occurrence"))]

We will only concern ourselves with one for now: @racket[syncheck:add-unused-require].

Although @racket[show-content] is the simplest interface to the functionality provided
by this module,
we will instead use the low-level API.
The reasons for doing so will hopefully become clear at the end.

The Low-Level API

- Explain @racket[syncheck-annotations<%>]

- Explain @racket[make-traversal]

make-traversal has a pretty complicated signature.
It can be simplified to this:

@defproc[(make-traversal [namespace namespace?])
         (values (-> syntax? void)
                 (-> void?))]{
                              
 The following code snippet demonstrates how it is supposed to be used.

 @racketblock[
 (define-values (supply-syntax done) (make-traversal namespace))
 (parameterize ([current-annotations some-syntax-annotations<%>])
   (supply-syntax program-under-analysis-stx))
 (done)]

 A call to @racket[(make-traversal namespace)] returns two functions:
 call them @racket[supply-syntax] and @racket[done].
 The first is supposed to be called on every syntax object that makes up the program
 (which is only one in our case).
 The second function is meant to be called when there are no more values to be supplied
 to @racket[supply-syntax].
}

@racket[make-traversal] does its job in collaboration with an instance of
@racket[syncheck-annotations<%>].

- Explain @racket[annotations-mixin]

There is an intricate interplay between quite a few things
in the next example.
First I'll show the whole code, and then I'll explain each bullet point.

- A simpler interface @racket[run-analysis]

@defproc[(run-analysis [namespace namespace?]
                        [ann       syncheck-annotations<%>]
                        [stx       syntax?])
         void?]{
 Implemented like this:

 @racketblock[
 (define (run-analysis namespace ann stx)
   (define-values (supply-syntax done) (make-traversal namespace #f)) (code:comment "The #f is an unused path argument")
   (parameterize ([current-annotations ann])
     (supply-syntax stx))
   (done))]
}

@examples[
 (require racket/class
          drracket/check-syntax)

 (define (run-analysis namespace ann stx)
   (define-values (supply-syntax done) (make-traversal namespace #f))
   (parameterize ([current-annotations ann])
     (supply-syntax stx))
   (done))

 (define stx
   (parameterize ([read-accept-reader #t]
                  [port-count-lines-enabled #t])
     (read-syntax "test.rkt" (open-input-string "#lang racket/base\n\n(require json)"))))

 (code:comment "(1) create the namespace for both expansion and analysis")
 (define base-namespace (make-base-namespace))

 (code:comment "(2) fully expand the program, ready to be analyzed by run-analysis")
 (define expanded-stx
   (parameterize ([current-namespace base-namespace])
     (expand stx)))

 (define ann
   (new (class (annotations-mixin object%)
          (super-new)
          (define/override (syncheck:find-source-object stx)
            stx)
          (code:comment "(3) test our ability to access annotations by printing the syntax object")
          (define/override (syncheck:add-unused-require stx start end)
            (println stx)))))

 (run-analysis base-namespace ann expanded-stx)]


@margin-note{
There is an awkward dependency between the arguments of run-analysis, the namespace passed
in must be the same as the one used to expand stx.}

As a side-effect of traversal @racketoutput{#<syntax:test.rkt:3:9 json>} was printed.
We now know we have access to the annotations.

Up until now we have been ignoring all the ways in which this process could go wrong.
In fact @racket[read-syntax] and @racket[expand] can fail in multiple ways.
Before we actually perform analysis, we should address the errors that can
happen in the @italic{read} and @italic{expand} phases.