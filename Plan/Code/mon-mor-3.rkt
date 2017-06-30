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
     (E-name e)))

(define ->name
  (reduction-relation
   Lambda-calculus
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)))

;; -----------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name 
(define (eval e)
  (first (apply-reduction-relation* ->name e)))

(define e3
  (term
   ((lambda (x) (lambda (y) x))
    ((lambda (x) x) z))))

(eval e3)
(traces ->name e3)


                      
     
     