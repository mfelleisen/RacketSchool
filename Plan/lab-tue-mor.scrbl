#lang scribble/manual

@(require "shared.rkt")
@(require (for-label redex))

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-tue-mor"]{@bold{Lab} The Mystery Languages of Records}

@goals[
@item{analyzing semantic variations of a syntactic language}
@item{modeling the semantics}
]

@;common[]

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language Records 
  (p ::= (prog f ... e))

  (f ::= (defun (x x) e))

  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (e + e)
     ;; strings
     s
     (empty? e)
     (e ++ e)
     ;; functions & let
     (function x)
     (e e)
     x
     (let ((x e)) e)
     ;; records
     {(s e) ...}
     (e |@| e))
  
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (x ::= variable-not-otherwise-mentioned)

  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))
))
@;%

@; -----------------------------------------------------------------------------
@exercise["ex:analyze"]{}

