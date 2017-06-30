#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "tue-mor"]{Modeling Functional Languages}

@goals[
@item{context-sensitive modeling}
@item{the power of Redex pattern matching}
]

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language PCF
  (p ::= 
     (prog (f ...) e))

  (f ::=
     (define x (lambda (x) e)))
     
  (e ::=
     x
     (lambda (x) e)
     (e e)

     ;; booleans
     tt
     ff
     (if e e e)

     ;; arithmetic
     n
     (e + e))
  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))
))
@;%
