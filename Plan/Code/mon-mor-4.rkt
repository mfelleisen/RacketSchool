#lang racket

(require redex)

;; -------------------------------------------------------
(define-language PCF
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
     (e + e))
  
  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language PCF)


















;; -------------------------------------------------------
(define-extended-language PCF-eval PCF
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

(define ->name
  (reduction-relation
   PCF-eval
   #:domain e
   (--> (in-hole E-name ((lambda (x) e_1) e_2))
        (in-hole E-name (substitute e_1 x e_2))
        beta-name)
   (--> (in-hole E-name (if tt e_1 e_2))
        (in-hole E-name e_1)
        if-tt)
   (--> (in-hole E-name (if ff e_1 e_2))
        (in-hole E-name e_2)
        if-ff)
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name ,(+ (term n_1) (term n_2)))
        plus)))
