#lang racket

(require redex)

;; ---------------------------------------------------------------------------------------------------
;; the syntax: a global letrec (called prog) with one body expression

(define-language PCF
  (p ::= 
     (prog (d ...) e))

  (d ::= (defvar x v))
  
  (v ::=
     tt
     ff
     n
     (lambda (x) e))

  (e ::=
     v  ;; <--- values
     (set! x e)
     x
     (e e)
     (if e e e)
     (e + e))

  (n ::= integer)
  (x ::= variable-not-otherwise-mentioned)
  
  #:binding-forms
  (prog ((defvar x_1 v) ...) #:refers-to (shadow x_1 ...) e #:refers-to (shadow x_1 ...))
  (lambda (x) e #:refers-to x))

(default-language PCF)

;; α equivalence works 
(test-equal (term (prog ((defvar x 1)) (x + x))) (term (prog ((defvar y 1)) (y + y))))

;; let's add a 'macro'
;; (let ((x_1 x_2)) e_1 e_2) binds the current value of x_2 to x_1,
;; evaluates e_1, throws away its value, and finally evaluates e_2 
(define-metafunction PCF
  let : ((x e)) e e -> e
  [(let ([x_lhs e_rhs]) e_1 e_2)
   ((lambda (x_lhs)
      ((lambda (x_d) e_2) e_1))
    e_rhs)
   (where (x_d) ,(variables-not-in (term (e_1 e_2)) '(dummy)))])

;; ---------------------------------------------------------------------------------------------------
;; the configuration syntax; all that's needed is P-level and E-level evaluation contexts 

(define-extended-language PCF-eval PCF
  (P ::=
     (prog (d ...) E))
  (E ::=
     hole
     (set! x E)
     (if E e e)
     (E e)
     (v E)
     (E + e)
     (v + E)))

;; ---------------------------------------------------------------------------------------------------
;; the reduction relation 

(define ->value
  (reduction-relation
   PCF-eval
   #:domain p
   #;
   (--> (in-hole P-value ((lambda (x) e_1) v_2))
        (in-hole P-value (substitute e_1 x v_2))
        beta-value)
   (--> (in-hole P (if tt e_1 e_2))
        (in-hole P e_1)
        if-tt)
   (--> (in-hole P (if ff e_1 e_2))
        (in-hole P e_2)
        if-ff)
   (--> (in-hole P (n_1 + n_2))
        (in-hole P ,(+ (term n_1) (term n_2)))
        plus)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E x))
        (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E v))
        retrieve)
   
   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E (set! x v_new)))
        (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...) (in-hole E 81))
        assignment)

   (--> (prog ((defvar x_1 v_1) ...) (in-hole E ((lambda (x) e) v)))
        (prog ((defvar x_1 v_1) ... (defvar x v)) (in-hole E e))
        allocation)))


;; ---------------------------------------------------------------------------------------------------

(define-extended-language PCF-deallocate PCF-eval
  (e ::=
     ....
     (deallocate x)))

(define ->dealloc
  (extend-reduction-relation
   ->value 
   PCF-deallocate
   #:domain p
   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E (deallocate x)))
        (prog ((defvar x_1 v_1) ...              (defvar x_2 v_2) ...) (in-hole E 81))
        de-allocation)))

;; ---------------------------------------------------------------------------------------------------
;; evaluate term e with the transitive closure of ->name 

(define-metafunction PCF-eval
  eval : p ->  v or closure or (err any)
  [(eval p) (unload ,(apply-reduction-relation* ->value (term p)))])

(define-metafunction PCF-eval
  unload : (p ...) -> v or closure or (err any)
  [(unload ((prog (d ...) (lambda (x) e)))) closure]
  [(unload ((prog (d ...) v)))              v]
  [(unload any)                             (err any)])

;; ---------------------------------------------------------------------------------------------------
;; tests

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

(test-equal (term (eval ,e3)) (term closure))
(test-equal (term (eval ,e4)) 3)
(test-equal (term (eval ,e5)) 42)

;; -------------------------------------------------------
(define e6
  (term 
   (prog ((defvar f (lambda (x) (if x g h)))
          (defvar g (lambda (x) 42))
          (defvar h (lambda (y) 21)))
         (let ([x 99])
           (set! f x)
           f))))

(test-equal (term (eval ,e6)) 99)

;; -------------------------------------------------------
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


;; ---------------------------------------------------------------------------------------------------
;; deallocation tests

(define e-d1
  (term
   (prog ()
         ((lambda (x)
            (let ([y (lambda (z) x)])
              (deallocate x)
              (y 10)))
          5))))

;; evaluate term e with the transitive closure of ->name 

(define-metafunction PCF-deallocate
  eval-da : p ->  v or closure or (err any)
  [(eval-da p) (unload ,(apply-reduction-relation* ->dealloc (term p)))])

#;
(define-metafunction PCF-eval
  unload : (p ...) -> v or closure or (err any)
  [(unload ((prog (d ...) (lambda (x) e)))) closure]
  [(unload ((prog (d ...) v)))              v]
  [(unload any)                             (err any)])

(term (eval-da ,e-d1))

;; NOW I need to decide how to model references to dangling pointers


(define-extended-language PCF-stack PCF-eval
  (e ::=
     ....
     (begin e_1 e_2)
     (deallocate x))
  (E ::=
     ....
     (begin E e)))

(define ->stack 
  (reduction-relation
   PCF-stack
   #:domain p
   (--> (in-hole P (if tt e_1 e_2))
        (in-hole P e_1)
        if-tt)
   (--> (in-hole P (if ff e_1 e_2))
        (in-hole P e_2)
        if-ff)
   (--> (in-hole P (n_1 + n_2))
        (in-hole P ,(+ (term n_1) (term n_2)))
        plus)

   (--> (in-hole P (begin v e))
        (in-hole P e)
        begin)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E x))
        (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E v))
        retrieve)
   
   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E (set! x v_new)))
        (prog ((defvar x_1 v_1) ... (defvar x v_new) (defvar x_2 v_2) ...) (in-hole E 81))
        assignment)

   (--> (prog ((defvar x_1 v_1) ...) (in-hole E ((lambda (x) e) v)))
        (prog ((defvar x_1 v_1) ... (defvar x v)) (in-hole E (begin e (deallocate x))))
        stack-allocation)

   (--> (prog ((defvar x_1 v_1) ... (defvar x v) (defvar x_2 v_2) ...) (in-hole E (deallocate x)))
        (prog ((defvar x_1 v_1) ...              (defvar x_2 v_2) ...) (in-hole E 81))
        de-allocation)))

(traces ->stack e-d1)
;; this one got stack because we returned a closure that referred to a stack-allocated variable
;; deallocate for stacks should really be 'pop', but it's okay because it acts like a stack discipline