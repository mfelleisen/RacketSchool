#lang racket

(require redex)

;; -----------------------------------------------------------------------------
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

(default-language PCF)

; (let ((x_1 x_2)) e_1 e_2) binds the current value of x_2 to x_1,
; evaluates e_1, throws away its value, and finally evaluates e_2 
(define-metafunction PCF
  let : ((x e)) e e -> e
  [(let ([x_lhs e_rhs]) e_1 e_2)
   ((lambda (x_lhs)
      ((lambda (x_dummy) e_2) (0 + e_1)))
    e_rhs)
   (where (x_dummy) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])

;; -----------------------------------------------------------------------------
(define-extended-language PCF-eval PCF
  (P-name ::=
          (prog (f ...) E-name))
  (E-name ::=
          hole
          (set! x E-name)
          (E-name e)
          (E-name + e)
          (v + E-name)))

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

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-name x))
        (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-name v))
        retrieve)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...)
              (in-hole E-name (set! x v_new)))
        (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...)
              (in-hole E-name v))
        set)

   (--> (prog ((defvar x v) ...) (in-hole E-name ((lambda (x_1) e) v_1)))
        (prog ((defvar x v) ... (defvar x_1 v_1)) (in-hole E-name e))
        (where (x_* ... x_1 x_& ...) (assignables e))
        allocate)))


(define-metafunction PCF
  assignables : e -> (x ...)
  [(assignables x) ()]
  [(assignables (set! x e)) (x x_1 ...) (where (x_1 ...) (assignables e))]
  [(assignables (void)) ()]
  [(assignables (lambda (x) e)) (assignables e)]
  [(assignables (e_1 e_2)) (x_1 ... x_2 ...)
                           (where (x_1 ...) (assignables e_1))
                           (where (x_2 ...) (assignables e_2))]
  [(assignables tt) ()]
  [(assignables ff) ()]
  [(assignables (if e_1 e_2 e_3)) (x_1 ... x_2 ...)
                                  (where (x_1 ...) (assignables e_1))
                                  (where (x_2 ...) (assignables e_2))
                                  (where (x_3 ...) (assignables e_3))]
  [(assignables n) ()]
  [(assignables (e_1 + e_2)) (x_1 ... x_2 ...)
                             (where (x_1 ...) (assignables e_1))
                             (where (x_2 ...) (assignables e_2))])
  
;; -----------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name 
(define-metafunction PCF-eval
  eval : p ->  v or error 
  [(eval p) v (where ((prog (f ...) v)) ,(apply-reduction-relation* ->name (term p)))]
  [(eval p)
   #;
   ,(apply-reduction-relation* ->name (term p))
   error])

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
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         ((f tt) 5))))

(term (eval ,e5))

(define e6
  (term 
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         (let ([x 99])
           (set! f x)
           f))))

(term (eval ,e6))
(traces ->name e6)
         

;; Let's diagnose the bug in this language model 
