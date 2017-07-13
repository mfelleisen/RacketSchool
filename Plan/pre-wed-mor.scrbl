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
     n
     tt
     ff
     (lambda (x) e))
     
  (e ::=
     
     (code:hilite(set! x e))
     
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
  Note @racket[(set! x e)] is not a binding construct. 

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
          (prog (f ...) E-value)))

(define-extended-language PCF-eval PCF
  (P-value ::=
           (prog (d ...) E-value))
  (E-value ::=
           hole
           (set! x E-value)
           (if E-value e e)
           (E-value e)
           (v E-value)
           (E-value + e)
           (v + E-value)))
))
@;%
In particular, we need a new evaluation context for @racket[set!].

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

@bold{Note} It would be easy to add deallocate rules and garbage-collection
rules as one-liners. To model stack de/allocation would be a bit harder but
still well within reach of Reduction Semantics. 

@bold{After the presentation} See @figure-ref{fig:setters} for the complete
reduction relation. @Figure-ref{fig:testing-set} shows the @tt{eval}
function and its test suite. If you run this model, you will see that a
test fails. Why? See the end of the file for the answer. 

@figure["fig:setters" @list{The complete reduction semantics for @tt{defvar}}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
(define ->value
  (reduction-relation
   PCF-eval
   #:domain p
   #;
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
              (in-hole E-value 81))
        assignment)

   (--> (prog ((defvar x_1 v_1) ...)
              (in-hole E-value ((lambda (x) e) v)))
        (prog ((defvar x_1 v_1) ... (defvar x v))
              (in-hole E-value e))
        allocation)))
))
@;%
}

@figure["fig:testing-set" @list{The @tt{eval} function for PCF plus @tt{defun}}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
;; evaluate term e with the transitive closure of ->name 
(define-metafunction PCF-eval
  eval : p ->  v or (err any)
  [(eval p)
   v
   (where ((prog (d ...) v))
          ,(apply-reduction-relation* ->value (term p)))]
  [(eval p)
   (err ,(apply-reduction-relation* ->value (term p)))])

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
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         ((f tt) 5))))

(test-equal (term (eval ,e3)) (term (lambda (y) 2)))
(test-equal (term (eval ,e4)) 3)
(test-equal (term (eval ,e5)) 42)


(define e6
  (term 
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         (let ([x 99])
           (set! f x)
           f))))

(test-equal (term (eval ,e6)) 99)

(define e7
  (term 
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         ((lambda (x)
            (let ([d (set! x 10)])
              (set! f (x + 89))
              f))
          42))))

(test-equal (term (eval ,e7)) 99)
))
@;%
}

@bold{Answer} The first test fails because the value inside of the final
@tt{prog} refers to a variable in the @tt{defvar} sequence. During our
discussions today, I often referred to @tt{load} and @tt{unload}
functions, which would make @tt{eval} much more realistic. In this example,
an @tt{unload} function would have to eliminate the @tt{defvar} references
in a safe manner. What could "safe" mean here? 
