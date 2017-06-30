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

(define e1 (term (lambda (x) y)))
(define e2 (term (lambda (z) y)))

(term (substitute ,e1 y x))
(term (substitute ,e2 y x))

;; -----------------------------------------------------------------------------
(define-extended-language Lambda-calculus Lambda
  (C ::=
     hole
     (lambda (x) C)
     (C e)
     (e C)))

(define ->beta
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole C ((lambda (x) e_1) e_2))
        (in-hole C (substitute e_1 x e_2))
        beta-name)))

(apply-reduction-relation ->beta (term (,e1 ,e2)))

(define e3
  (term
   ((lambda (x) (lambda (y) x))
    ((lambda (x) x) z))))

(apply-reduction-relation ->beta e3)
(apply-reduction-relation* ->beta e3)
(traces ->beta e3)


                      
     
     