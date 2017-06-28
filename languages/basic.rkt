#lang racket

(require redex)
(require "testing.rkt")

(provide (all-defined-out))

;; A basic language that all of the mystery languages will build upon.

;; ---------------------------------------------------------------------------------------------------
;; syntax

(define-language basic-syntax
  (p ::= (prog f ... e))
  (f ::= (defun (x x) e))
  (e ::=
     ;; booleans
     b
     (if e e e)
     ;; numbers
     n
     (zero? e)
     (e + e)
     ;; strings
     s
     (empty? e)
     (e ++ e)
     ;; functions & let
     (function x)
     (e e)
     x
     (let ((x e)) e))
  (x ::= variable-not-otherwise-mentioned)
  (b ::= true false)
  (n ::= number)
  (s ::= string)
  (v ::=
     b
     n
     s
     (function x))
  #:binding-forms
  (let ((x e_1)) e_2 #:refers-to x))
  

;; ---------------------------------------------------------------------------------------------------
;; evaluation

(define-extended-language basic-lang basic-syntax
  (P ::= (prog f ... E))
  (E ::=
     hole
     ;; booleans
     (if E e e)
     ;; numbers
     (zero? E)
     (E + e)
     (v + E)
     ;; strings
     (empty? E)
     (E ++ e)
     (v ++ E)
     ;; functions & let
     (E e)
     (v E)
     (let ((x E)) e)))

(define basic->
  (reduction-relation basic-lang
   ;; booleans
   (--> (in-hole P (if true e_then e_else))
        (in-hole P e_then)
        e-if-true)
   (--> (in-hole P (if false e_then e_else))
        (in-hole P e_else)
        e-if-false)
   ;; numbers
   (--> (in-hole P (zero? 0))
        (in-hole P true)
        e-zero-yes)
   (--> (in-hole P (zero? n))
        (in-hole P false)
        (side-condition (not (equal? (term n) 0)))
        e-zero-no)
   (--> (in-hole P (n_1 + n_2))
        (in-hole P ,(+ (term n_1) (term n_2)))
        e-plus)
   ;; strings
   (--> (in-hole P (empty? ""))
        (in-hole P true)
        e-empty-yes)
   (--> (in-hole P (empty? s))
        (in-hole P false)
        (side-condition (not (equal? (term s) "")))
        e-empty-no)
   (--> (in-hole P (s_1 ++ s_2))
        (in-hole P ,(string-append (term s_1) (term s_2)))
        e-append)
   ;; termination
   (--> (prog f ... v)
        v
        e-halt)
   ;; id
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun)))
        e-id)
   ;; let
   (--> (in-hole P (let ((x v)) e))
        (in-hole P (substitute e x v))
        e-let)
   ;; apply
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_body x_param v_arg)))
        e-apply)))


;; ---------------------------------------------------------------------------------------------------
;; tests

(define-test ex-bool-1
  (prog (if true "yes" "no"))
  "yes")

(define-test ex-bool-2
  (prog (if false "yes" "no"))
  "no")

(define-test ex-str-1
  (prog (empty? ""))
  true)

(define-test ex-str-2
  (prog (empty? " "))
  false)

(define-test ex-str-3
  (prog ("abc" ++ "def"))
  "abcdef")

(define-test ex-record-1
  (prog {("x" ("a" ++ "b")) ("y" (empty? ""))})
  {("x" "ab") ("y" true)})

(define-test ex-record-2
  (prog ({("x" true) ("y" false)} @ "x"))
  true)

(define-test ex-record-3
  (prog ({("x" true) ("y" false)} @ "y"))
  false)

(define-test ex-num-1
  (prog (zero? 0))
  true)

(define-test ex-num-2
  (prog (zero? -1))
  false)

(define-test ex-num-3
  (prog (1 + 2))
  3)

(define-test ex-defun-1
  (prog (defun (f x) (x + 1)) (f 2))
  3)

(define-test ex-defun-2
  (prog (defun (f x) (x + 1))
        (defun (g x) (x + 2))
        (defun (h x) (x + 3))
        (g 1))
  3)

(define-test ex-defun-3
  (prog (defun (even? x) (if (zero? x) true (odd? (x + -1))))
        (defun (odd? x) (if (zero? x) false (even? (x + -1))))
        (even? 3))
  false)

(define-test ex-defun-4
  (prog (defun (f x) (x + 1))
        (1 + (f 1)))
  3)

(define-test ex-defun-5
  (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))
  15)

(define-test ex-defun-6
  (prog (defun (twice f) (f (f 1)))
        (defun (inc x) (x + 1))
        (twice inc))
  3)

(define-test ex-let-1
  (prog (let ((x 3)) x)) 3)

(define-test ex-let-2
  (prog (let ((x 1)) (let ((x (x + 1))) (x + 1))))
  3)

(define-test ex-let-3
  (prog (1 + (let ((x 1)) (x + 1))))
  3)

(define-syntax-rule (run-bool-tests lang) (run-tests lang ex-bool-1 ex-bool-2))
(define-syntax-rule (run-str-tests lang)  (run-tests lang ex-str-1 ex-str-2 ex-str-3))
(define-syntax-rule (run-num-tests lang)  (run-tests lang ex-num-1 ex-num-2 ex-num-3))
(define-syntax-rule (run-func-tests lang) (run-tests lang ex-defun-1 ex-defun-2 ex-defun-3 ex-defun-4 ex-defun-5 ex-defun-6))
(define-syntax-rule (run-let-tests lang)  (run-tests lang ex-let-1 ex-let-2 ex-let-3))

(define-syntax-rule
  (run-standard-tests lang)
  (begin (run-bool-tests lang)
         (run-str-tests lang)
         (run-num-tests lang)
         (run-func-tests lang)
         (run-let-tests lang)))

(module+ test (run-standard-tests basic->))
