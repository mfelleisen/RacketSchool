#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "wed-mor"]{Modeling Imperative-Functional Languages}

@goals[
@item{revise the language for assignment statements}
]

@section{Syntax for Variable Assignment}

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language PCF
  (p ::= 
     (prog (f ...) e))

  (f ::=
     (defvar x v))

  
  (v ::=
     (void)
     n
     tt
     ff
     (lambda (x) e))
     
  (e ::=
     
     (code:hilite(set! x e))
     (code:hilite (void))
     
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
  Note @racket[(set! x e)] is not a binding construct. The addition of
  @racket[(void)] is not necessary but it makes assignments act like those
  in Racket. 

@section{Blocks Anybody?}

For writing programs in this world, you'd also want blocks or local
declarations. But we don't add those as syntax but as meta-functions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-metafunction PCF
  let : ((x e)) e e -> e
  [(let ([x_lhs e_rhs]) e_1 e_2)
   ((lambda (x_lhs)
      ((lambda (x_dummy) e_2) e_1))
    e_rhs)
   (where (x_dummy) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])
))
@;%

Here are some sample programs: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define e1
  (term
   (lambda (x)
     (lambda (y)
       (let ([tmp x])
         (set! x (+ y 1))
         tmp)))))

(define p-1 (term ((,e1 1) 2)))

(define e2
  (term
   ((lambda (x)
      (let ([tmp x])
        (set! x y)
        tmp))
    (let ([tmp-z z])
      (set! z (+ z 1))
      (let ([tmp-y y])
        (set! y tmp-z)
        tmp-y)))))

(define p-2
  (term ((lambda (y) ((lambda (z) ,e2) 1)) 2)))
))
@;%
How do they behave? 

@; -----------------------------------------------------------------------------
@section{Reduction Rules for Imperative PCF}

Today we use the call-by-value PCF to add variable assignment---simply for
variety not because there is anything inherently wrong about assignment
statements and call-by-name. 

From PCF with function definitions, we know we need evaluation contexts for
both expressions and programs: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-extended-language PCF-eval PCF
  (P-value ::=
          (prog (f ...) E-value))
  (E-value ::=
          hole
          (set! x E-value)
          (E-value e)
          (v E-value)
          (E-value + e)
          (v + E-value)))
))
@;%

Looking up variable values works just like looking up functions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
           (in-hole E-value x))
     (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
           (in-hole E-value v))
     retrieve)
))
@;%

And this tells us that modifying what a variable stands for works in a
similar manner: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
           (in-hole E-value (set! x v_new)))
     (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...)
           (in-hole E-value v))
     set)
))
@;%

But to make it all work, we need one more rule: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(--> (prog ((defvar x v) ...) (in-hole E-value ((lambda (x_1) e) v_1)))
     (prog ((defvar x v) ... (defvar x_1 v_1)) (in-hole E-value e))
     (where (x_* ... x_1 x_& ...) (assignables e))
     allocate)
))
@;%
We call this allocate because in a language such as Racket @racket[lambda]
allocates a slot in the store for assignable variable bindings. 

