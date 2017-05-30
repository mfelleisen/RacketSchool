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
(define ex-rec (term {("one" "hell") ("two" "no")}))
(define ex-@ (term (,ex-rec @ "two")))

;; using lambda to name records 
(define ex3 (term ((λ (r) ((r @ "two") ++ (r @ "one"))) ,ex-rec)))

;; ---------------------------------------------------------------------------------------------------
;; substitution (default)

(module+ test
  (default-language Λ-with-records)
  (test-equal (term (substitute (λ (x) y) y "hell")) (term (λ (x) "hell")))
  (test-equal (term (substitute (λ (x) y) y (x "hell"))) (term (λ (x_1) (x "hell")))))

;; ---------------------------------------------------------------------------------------------------
;; run-time 

(define-extended-language with-records Λ-with-records
  (v ::=
     s
     {(s v) ...}
     (λ (x) e))
  (E ::=
     hole
     ;; records 
     {(s v) ... (s E) (s e) ...}
     (E @ s)
     ;; strings
     (if-empty E e e)
     (E ++ e)
     (v ++ E)
     ;; standard material 
     (E e)
     (v E)))

(module+ test
  (test--> ->records ex-tt "hell")
  (test--> ->records ex-ff "no")
  (test--> ->records ex-@ "no")
  (test-->> ->records ex3 "nohell"))

(define ->records
  (reduction-relation with-records
   (--> (in-hole E ((λ (x) e) v)) (in-hole E (substitute e x v)))
   (--> (in-hole E (if-empty "" e_then e_else)) (in-hole E e_then))
   (--> (in-hole E (if-empty s e_then e_else)) (in-hole E e_else)
        (side-condition (not (equal? (term s) ""))))
   (--> (in-hole E ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s)) (in-hole E v))
   (--> (in-hole E (s_1 ++ s_2)) (in-hole E ,(string-append (term s_1) (term s_2))))))
