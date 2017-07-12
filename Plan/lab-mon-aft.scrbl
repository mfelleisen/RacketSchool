#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-mon-aft"]{@bold{Lab} Modeling PCF-value}

@goals[
@item{turn your PCF-xyz code into a model}
@item{... with a conditionalized evaluator} 
@item{... with tests for the reduction relation and the meta-functions}
]

@;common[]

@section[#:tag "lma" #:style 'unnumbered]{Exercises}

Feel free to copy any code you need from the lecture notes. 

@exercise["ex:plus"]{Define a version of PCF where @racket[plus] works on
natural numbers only. It works on those exactly like Racket.}

@exercise["ex:minus"]{Equip PCF with @racket[minus] which works on
natural numbers only. When the second argument is too large, @racket[minus]
produces 0.}

@exercise["ex:lookup"]{Design @racket[lookup]. The metafunction consumes a
variable and an @tech{environment}. It determines the leftmost expression
associated with the variable; otherwise it produces @racket[#false].

Here is the definition of @deftech{environment}: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-extended-language Env Lambda
  (e ::= .... natural)
  (env ::= ((x e) ...)))
))
@;%
 The language extension also adds numbers of the sub-language of
 expressions. 

Before you get started, make sure you can create examples of
@tech{environment}s and confirm their well-formedness.}

@exercise["ex:let"]{Develop the metafunction @racket[let], which extends
 the language with a notational shorthand, also known as syntactic sugar. 

 Once you have this metafunction, you can write expressions such as 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(term 
  (let ((x (lambda (a) (lambda (b) a))))
    ((x y) y)))
))
@;%
 Like Racket's @racket[let], the function elaborates surface syntax into
 core syntax:
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(term 
  ((lambda (x) ((x y) y))
   (lambda (a) (lambda (b) a))))
))
@;%

 Since this elaboration happens as the term is constructed, all other
 metafunctions work as expected on this extended syntax. For example,
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(term 
 (substitute 
  (let ((x (lambda (a) (lambda (b a)))))
    ((x y) y))
  y 
  (lambda (x) x)))
))
@;%
 produces the expected result. What is that? Formulate a test case.}

@exercise["ex:sd"]{Develop the static-distance function for Lambda. 

First, extend Lambda with the form @racket[(sd n)] where @racket[n] stands
for naturals. 

Second, define the meta-function @racket[sd*]. It consumes a term in plain
Lambda and delivers one in the extended language where all bound variable
references are replaced with @racket[(sd n)] where @racket[n] counts the
number of @racket[lambda]s between the bound occurrence of the variable and
the binding occurrence.

Third, start by formulating examples/tests.

Fourth, this exercise is hard if you did not go through an introduction to
systematic function design (and even then it might be hard). Give it a try,
but don't get frustrated if you don't finish it today. 

See @figure-ref{sd} for the solution when you're done.}


@figure["sd" @list{The static distance function}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
;; compute static distance version of Lambda terms 

(define-language Lambda
  (e ::=
     x
     (lambda (x) e)
     (e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(define-extended-language SD Lambda
  (e ::=
     ....
     (sd n))
  (n ::=
     natural))

(default-language SD)

;; the desired function (dispatched to the function that can do the work)
(define-metafunction SD
  sd* : e -> e
  [(sd* e) (sd*-accu e ())])

;; (x ...) the binding variables on path from root
;; in reverse order
(define-metafunction SD
  sd*-accu : e (x ...) -> e
  [(sd*-accu x (x_1 ... x x_2 ...))
   (sd ,(length (term (x_1 ...))))]
  [(sd*-accu x any) x]
  [(sd*-accu (lambda (x) e) (x_1 ...))
   (lambda (x) (sd*-accu e (x x_1 ...)))]
  [(sd*-accu (e_1 e_2) (x ...))
   ((sd*-accu e_1 (x ...)) (sd*-accu e_2 (x ...)))])

;; tests 
(test-equal (term (sd* c))
            (term c))
(test-equal (term (sd* (lambda (y) y)))
            (term (lambda (y) (sd 0))))
(test-equal (term (sd* (lambda (y) (lambda (x) (x y)))))
            (term (lambda (y) (lambda (x) ((sd 0) (sd 1))))))
(test-equal (term (sd* (lambda (y) (lambda (x) z))))
            (term (lambda (y) (lambda (x) z))))
(test-equal (term (sd* (lambda (y) (y (lambda (x) (x y))))))
            (term
             (lambda (y)
               ((sd 0) (lambda (x) ((sd 0) (sd 1)))))))
(test-equal (term (sd* (lambda (x) (lambda (x) x))))
            (term (lambda (x) (lambda (x) (sd 0)))))
))
@;%
}
