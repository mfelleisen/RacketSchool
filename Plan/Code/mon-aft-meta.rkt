#lang racket

(require redex)

;; -----------------------------------------------------------------------------
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

;; -----------------------------------------------------------------------------
(define-extended-language PCF-eval PCF
  (E-name ::=
     hole
     (E-name e)
     (E-name + e)
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
   (--> (in-hole E-name (if v e_1 e_2))
        (in-hole E-name e_1)
        (where tt (falsify v))
        if-tt)
   (--> (in-hole E-name (if v e_1 e_2))
        (in-hole E-name e_2)
        (where ff (falsify v))
        if-ff)
   (--> (in-hole E-name (n_1 + n_2))
        (in-hole E-name (plus n_1 n_2))
        plus)))

(define-metafunction PCF-eval
  plus : n n -> n
  [(plus 1 1) 2]
  [(plus n n) 3]
  [(plus n_1 n_2) 1])

(define-metafunction PCF-eval
  plus-racket-meta : n n -> n
  [(plus-racket-meta n_1 n_2) ,(+ (term n_1) (term n_2))])

(define-metafunction PCF-eval
  falsify : v -> tt or ff
  [(falsify 0) ff]
  [(falsify ff) ff]
  [(falsify v) tt])

;; -----------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name
(define-metafunction PCF-eval
  eval : e -> v
  [(eval e) v
   (where (v) ,(apply-reduction-relation* ->name (term e)))])

(define e3
  (term
   ((lambda (x) (lambda (y) x))
    ((lambda (x) x) z))))

(define e4
  (term
   (((lambda (x) (lambda (y) x))
     (1 + (1 + 1)))
    (2 + 2))))

(term (eval ,e3))
(traces ->name e3)

(term (eval ,e4))
(traces ->name e4)

;; Let's diagnose the bug in this language model 
