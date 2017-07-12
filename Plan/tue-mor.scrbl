#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "tue-mor"]{Modeling Functional Languages}

@goals[
@item{context-sensitive modeling}
@item{the power of Redex pattern matching}
]

@section{PCF and Global Function Definitions}

Say you want to add global, mutually recursive function definitions to
PCF. Here is a simple way of extending the syntax: 

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
     (+ e e))
  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))
))
@;%

Stop! When you revise a model, first get all the test cases to work
again. Here you will need to change E contexts to P contexts, that is, 
@tt{prog}s with holes in the expression position. 

Then. Come up with an program that gets stuck because it uses the new
syntactic features. 

@; -----------------------------------------------------------------------------
@section{Context-sensitive Rules} 

Your revised model gets stuck because the tree root of any program now have
@tt{prog} as its marker. We need a new kind of context. 

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-extended-language PCF-eval PCF
  (P-name ::=
          (prog (f ...) E-name))

  (E-name ::=
          hole
          (E-name e)
          (E-name + e)
	  (if E-name e e)
          (v + E-name))

  (v ::=
     n
     tt
     ff
     (lambda (x) e)))
))
@;%

Your revised model gets stuck because expressions now have free
variables. These free variables point to function definitions and show up
in the hole of an E context.

Now we have two options:
@itemlist[#:style 'ordered 

@item{Use the lookup function from yesterday's lab.}

@item{Exploit Redex's extremely powerful pattern matching.}
]

We use option 2: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(--> (prog ((define x_1 v_1) ... (define x v) (define x_2 v_2) ...)
           (in-hole E-name x))
     (prog ((define x_1 v_1) ... (define x v) (define x_2 v_2) ...)
           (in-hole E-name v)))
))
@;%

@; -----------------------------------------------------------------------------
@section{Exercises}

@exercise["ex:recursion"]{Check that the model works for a simple recursion
function. No, you won't get very far with the recursion we have.}

@exercise["ex:cs-look"]{Implement option 1.}

@exercise["ex:value"]{Revise your call-by-value model of PCF to include
global definitions.}
