#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "wed-mor"]{Modeling Imperative-Functional Languages}

@goals[
@item{revise the language for assignment statements}
]

@section{Variable Assignment}

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
     
     (set! x e)
     (void)
     
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
      ((lambda (x_dummy) e_2) (0 + e_1)))
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

(define ->value
  (reduction-relation
   PCF-eval
   #:domain p
   (--> (in-hole P-value ((lambda (x) e_1) v_2))
        (in-hole P-value (substitute e_1 x v_2))
        beta-value)
   (--> (in-hole P-value (if tt e_1 e_2))
        (in-hole P-value e_1)
        if-tt)
   (--> (in-hole P-value (if ff e_1 e_2))
        (in-hole P-value e_2)
        if-ff)
   (--> (in-hole P-value (n_1 + n_2))
        (in-hole P-value ,(+ (term n_1) (term n_2)))
        plus)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-value x))
        (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-value v))
        retrieve)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-value (set! x v_new)))
        (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...)
              (in-hole E-value v))
        set)

   (--> (prog ((defvar x v) ...) (in-hole E-value ((lambda (x_1) e) v_1)))
        (prog ((defvar x v) ... (defvar x_1 v_1)) (in-hole E-value e))
        (where (x_* ... x_1 x_& ...) (assignables e))
        allocate)))
))
@;%
