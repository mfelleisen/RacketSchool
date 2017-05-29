#lang racket

(require redex)

;; ---------------------------------------------------------------------------------------------------
(define-language Λ-with-records
  (e ::=
     ;; records 
     {(s e) ...}
     (e @ s)
     ;; strings
     s
     (if-empty e e e)
     (e ++ e)
     ;; standard material 
     x
     (e e)
     (λ (x) e))
  (s ::=
     string)
  (x ::=
     variable-not-otherwise-mentioned)
  #:binding-forms 
  (λ (x) e #:refers-to x))

;; ---------------------------------------------------------------------------------------------------
;; some examples 

;; two String conditionals 
(define ex-tt (term (if-empty "" "hell" "no")))
(define ex-ff (term (if-empty "way to go" "hell" "no")))

;; dealing with records 
(define ex-rec (term {("one" ("hell")) ("two" "no")}))
(define ex-@ (term (,ex-rec @ "two")))

;; using lambda to name records 
(define ex3 (term ((λ (r) ((r @ "two") ++ (r @ "one"))) ,ex-rec)))

;; ---------------------------------------------------------------------------------------------------
;; substitution

(module+ test
  (default-language Λ-with-records)
  (test-equal (term (substitute (λ (x) y) y "hell")) (term (λ (x) "hell")))
  (test-equal (term (substitute (λ (x) y) y (x "hell"))) (term (λ (x_1) (x "hell")))))

(module+ test
  (test-equal (term (subst (λ (x) y) y "hell")) (term (λ (x) "hell")))
  (test-equal (term (subst (λ (x) y) y (x "hell"))) (term (λ (x_1) (x "hell")))))

(define-metafunction Λ-with-records
  subst : e x e -> e
  [(subst x x e) e]
  [(subst {(s_1 e_1) ...} x e) {(s_1 (subst e_1 x e)) ...}]
  [(subst (e_r @ s) x e) ((subst e_r x e) @ s)]
  [(subst s x e) s]
  [(subst (if-empty e_1 e_2 e_2)) (if-empty (subst e_1 x e) (subst e_2 x e) (subst e_3 x e))]
  [(subst (e_1 ++ e_2) x e) ((subst e_1 x e) ++ (subst e_2 x e))]
  [(subst x_y x e) e]
  [(subst (e_f e_a) x e) ((subst e_f x e) (subst e_a x e))]
  [(subst (λ (x) e) x e_v) (λ (x) e)]
  [(subst (λ (x) e) x_y e_v) (λ (x) (subst e x_y e_v))])

