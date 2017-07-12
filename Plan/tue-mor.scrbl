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
The complete reduction relation is available in @figure-ref{defun};
@figure-ref{defun-testing} shows how to revise the @tt{eval} function plus
its test suite. 

@; -----------------------------------------------------------------------------
@section{Exercises}

@exercise["ex:recursion"]{Check that the model works for a simple recursion
function. No, you won't get very far with the recursion we have.}

@exercise["ex:cs-look"]{Implement option 1.}

@exercise["ex:value"]{Revise your call-by-value model of PCF to include
global definitions.}

@figure["defun" @list{The complete reduction relation for @tt{defun}}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
(define ->name
  (reduction-relation
   PCF-eval
   #:domain p
   (--> (in-hole P-name ((lambda (x) e_1) e_2))
        (in-hole P-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole P-name (if tt e_1 e_2))
        (in-hole P-name e_1)
        if-tt)
   (--> (in-hole P-name (if ff e_1 e_2))
        (in-hole P-name e_2)
        if-ff)
   (--> (in-hole P-name (n_1 + n_2))
        (in-hole P-name ,(+ (term n_1) (term n_2)))
        plus)
   (--> (prog (d ... (defun (x x_y) e) d_2 ...)
              (in-hole E-name x))
        (prog (d ... (defun (x x_y) e) d_2 ...)
              (in-hole E-name (lambda (x_y) e))))))
))
@;%
}

@figure["defun-testing" @list{The @tt{eval} function for PCF plus @tt{defun}}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
;; evaluate term e with the transitive closure of ->name
(define-metafunction PCF-eval
  eval : p -> v or (shu any)
  [(eval p)
   v_one-term 
   (where ((prog (d ...) v_one-term))
          ,(apply-reduction-relation* ->name (term p)))]
  [(eval p)
   (shu
    ,(apply-reduction-relation* ->name (term p)))])

(define e3
  (term
   (prog ()
         ((lambda (x) (lambda (y) x))
          ((lambda (x) x) 2)))))

(define e4
  (term
   (prog ()
         (((lambda (x) (lambda (y) x))
           (1 + (1 + 1)))
          (2 + 2)))))

(define e5
  (term
   (prog ((defun (h why) (why + 1)))
         42)))

(define e6
  (term
   (prog ((defun (h why) (why + 1)))
         ((h 31) + 2))))
          
(test-equal (term (eval ,e6))
            34)
(test-equal (term (eval ,e5))
            42)
(test-equal (term (eval ,e3))
            (term (lambda (y) ((lambda (x) x) 2))))
(test-equal (term (eval ,e4))
            3)
))
@;%
}
