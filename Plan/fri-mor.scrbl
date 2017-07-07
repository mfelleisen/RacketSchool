#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title[#:tag "fri-mor"]{Specification vs Implementation} 

@goals[
 @item{building a language from specification}
 @item{testing languages against specifications}
]

@section{Setting up a test bed}

@(require "fri-diagram.rkt")

@figure["fig:testing" @list{Comparing specifications and implementations}]{
  @impl-vs-spec[.6]}

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-namespace-anchor my-ns-anchor)

(define (run-in-mylang module-str binding)
 (parameterize ([read-accept-lang #t]
                [read-accept-reader #t]
                [current-namespace (namespace-anchor->namespace
                                    my-ns-anchor)])
   (eval
    (call-with-input-string
    module-str
    (λ (p) (read-syntax 'zz p))))
   (dynamic-require ''anonymous-module 'f)))

(run-in-mylang "#lang racket
(provide (all-defined-out))
(define f 13)" 'f)
))
@;%


