#lang racket

(require redex)

;; -----------------------------------------------------------------------------
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
     (e + e))
  (n ::=
     integer)

  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language PCF)

;; -----------------------------------------------------------------------------
(define-extended-language PCF-eval PCF
  (P-name ::=
          (prog (f ...) E-name))
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

   (--> (prog ((define x_1 v_1) ... (define x v) (define x_2 v_2) ...)
              (in-hole E-name x))
        (prog ((define x_1 v_1) ... (define x v) (define x_2 v_2) ...)
              (in-hole E-name v))
        retrieve)))

;; -----------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name 
(define-metafunction PCF-eval
  eval : p ->  v or error 
  [(eval p) v
   (where ((prog (f ...) v)) ,(apply-reduction-relation* ->name (term p)))]
  [(eval p) error])

(define e3
  (term
   (prog ()
         ((lambda (x) (lambda (y) x))
          ((lambda (x) x) z)))))

(define e4
  (term
   (prog ()
         (((lambda (x) (lambda (y) x))
           (1 + (1 + 1)))
          (2 + 2)))))

(term (eval ,e3))
(traces ->name e3)

(term (eval ,e4))
(traces ->name e4)

(define e5
  (term 
   (prog ((define f (lambda (x) (if x g h)))
          (define g (lambda (x) 42))
          (define h (lambda (y) 21)))
         ((f tt) 5))))

(term (eval ,e5))
         

;; Let's diagnose the bug in this language model 
