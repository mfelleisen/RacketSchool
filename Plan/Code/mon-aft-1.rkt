#lang racket

(require redex)

;; -----------------------------------------------------------------------------
(define-language Lambda
  (e ::=
     x
     (lambda (x) e)
     (e e))

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language Lambda)

;; -----------------------------------------------------------------------------
(define-extended-language Lambda-calculus Lambda
  (E-name ::=
     hole
     (E-name e))
  (v ::=
     (lambda (x) e)))

(define ->name
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)))

;; -----------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name 
(define-metafunction Lambda-calculus
  eval : e -> v
  [(eval e) ,(first (apply-reduction-relation* ->name (term e)))])

(define e3
  (term
   ((lambda (x) (lambda (y) x))
    ((lambda (x) x) z))))

(define e3-evaluated
  (term
   (lambda (y) ((lambda (x) x) z))))

(traces ->name e3)

(term (eval ,e3))
e3-evaluated

;; -----------------------------------------------------------------------------
(define-metafunction Lambda-calculus
  eval-2 : e -> v or unbound-variables
  [(eval-2 e)
   ,(first (apply-reduction-relation* ->name (term e)))
   (where () (fv e))]
  [(eval-2 e)
   unbound-variables])

(define-metafunction Lambda-calculus
  fv : e -> (x ...)
  [(fv x) (x)]
  [(fv (e_1 e_2)) (union (fv e_1) (fv e_2))]
  [(fv (lambda (x) e)) (minus (fv e) x)])

(define-metafunction Lambda-calculus
  union : (x ...) (x ...) -> (x ...)
  [(union (x_1 ...) (x_2 ...)) (x_1 ... x_2 ...)])

(define-metafunction Lambda-calculus
  minus : (x ...) x -> (x ...)
  [(minus (x_1 ... x x_2 ...) x) (minus (x_1 ... x_2 ...) x)]
  [(minus (x_1 ...) x) (x_1 ...)])

(test-equal (term (minus () x)) (term ()))
(test-equal (term (minus (x) x)) (term ()))
(test-equal (term (minus (x y) x)) (term (y)))
(test-equal (term (minus (z w x y) x)) (term (z w y)))

(test--> ->name e3 e3-evaluated)
(test--> ->name e3 (term z))

(term (eval-2 ,e3))