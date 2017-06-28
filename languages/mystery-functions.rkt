#lang racket

(require redex)
(require "basic.rkt")
(require "testing.rkt")

(provide func->1 func->2 func->3)


;; ---------------------------------------------------------------------------------------------------
;; language 1: Regular function calls

(define-extended-language func-lang-1 basic-lang)

(define func->1 (extend-reduction-relation basic-> func-lang-1))


;; ---------------------------------------------------------------------------------------------------
;; language 2: COBOL-style "recursion": each function has a single statically-allocated return address

; These syntax extensions are meant to be internal, and thus start with '%'.
(define-extended-language func-syntax-2 basic-lang
  (p ::= (prog f ... uf ... e))
  (uf ::= (defun (x x) e)) ; uninitialized functions
  (f ::= (%defun e (x x) e)) ; initialized functions
  (e ::= ....
     (%return x e)))

(define-extended-language func-lang-2 func-syntax-2
  (P ::= (prog f ... E))
  (E ::= ....
     (%return x E)))

(define func->2
  (extend-reduction-relation basic-> func-lang-2
   ;; id (standard)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun)))
        e-id)
   ;; init
   (--> (prog (defun (x_f1 x_p1) e_1) (defun (x_f2 x_p2) e_2) ... e)
        (prog (%defun 0 (x_f1 x_p1) e_1) (%defun 0 (x_f2 x_p2) e_2) ... e)
        e-init)
   ;; apply
   (--> (prog f_1 ... (%defun e_1 (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (%defun (in-hole E continue) (x_fun x_param) e_body) f_2 ...
              (%return x_fun (substitute e_body x_param v_arg)))
        e-apply)
   ;; COBOL-style return
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (%return x_fun v)))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (substitute e_cont continue v))) ;keep the context E?
        e-return)))


;; ---------------------------------------------------------------------------------------------------
;; language 3: Function calls are gotos!

(define-extended-language func-lang-3 basic-lang)

(define func->3
  (extend-reduction-relation basic-> func-lang-3
   ;; apply
   (--> (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (defun (x_fun x_param) e_body) f_2 ...
              (substitute e_body x_param v_arg))
        e-apply)))


;; ---------------------------------------------------------------------------------------------------
;; language 4: Mystery semantics!
;;             How does this language differ from language 1?
;;             Find a program that will demonstrate the difference.

(define-extended-language func-syntax-4 basic-lang
  (p ::= (prog f ... uf ... e))
  (uf ::= (defun (x x) e))
  (f ::= (%defun e (x x) e))
  (e ::= ....
     (%call x e)
     (return e))) ; Users can write "return" inside function definitions

(define-extended-language func-lang-4 func-syntax-4
  (P ::= (prog f ... E))
  (E ::= ....
     (%call x E)
     (return E)))

(define func->4
  (extend-reduction-relation basic-> func-lang-4
   ;; id (standard)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E x_fun))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (function x_fun)))
        e-id)
   ;; init
   ; [TODO] replace 0 with something more sensible, and eliminate one of the 'apply' rules?
   (--> (prog (defun (x_f1 x_p1) e_1)
              (defun (x_f2 x_p2) e_2) ... e_main)
        (prog (%defun 0 (x_f1 x_p1) e_1)
              (%defun 0 (x_f2 x_p2) e_2) ... e_main)
        e-init)
   ;; apply
   (--> (prog f_1 ... (%defun 0 (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (%defun 0 (x_fun x_param) e_body) f_2 ...
              (in-hole E (%call x_fun (substitute e_body x_param v_arg))))
        e-apply)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (%call x_fun (substitute e_cont continue v_arg))))
        (side-condition (not (equal? (term e_cont) 0)))
        e-apply-2)
   ;; return
   (--> (prog f ... (in-hole E (%call x_fun v)))
        (prog f ... (in-hole E v))
        e-return)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E_1 (%call x_fun (in-hole E_2 (return v)))))
        (prog f_1 ... (%defun (in-hole E_2 continue) (x_fun x_param) e_body) f_2 ...
              (in-hole E_1 v))
        e-return-2)))


;; ---------------------------------------------------------------------------------------------------
;; tests

;; triangular numbers
(define ex-tri (term (prog (defun (tri n) (if (zero? n) 0 (n + (tri (n + -1))))) (tri 5))))

;; passing functions
(define ex-twice
  (term (prog (defun (twice f) (f (f 1)))
              (defun (inc x) (x + 1))
              (twice inc))))

(module+ test
  (run-standard-tests func->1)
  (test-->> func->1 ex-tri 15)
  (test-->> func->1 ex-twice 3))

(module+ test
  (run-bool-tests func->2)
  (run-str-tests func->2)
  (run-num-tests func->2)
  (run-let-tests func->2)
  (test-->> func->2 ex-twice 3))

(module+ test
  (run-bool-tests func->3)
  (run-str-tests func->3)
  (run-num-tests func->3)
  (run-let-tests func->3)
  (test-->> func->3 ex-tri 0)
  (test-->> func->3 ex-twice 2))


;; ---------------------------------------------------------------------------------------------------
;; tests (SPOILERS!)

(define-test ex-gen-1
  (prog (defun (f x) ((return 1) + (return 2))) ((f 0) + (f 0)))
  3)

(define-test ex-gen-2
  (prog (defun (f x) ((return 1) + (return 2))) (((f 0) + (f 0)) + (f 0)))
  3)

(module+ test
  (run-standard-tests func->4)
  (run-test func->4 ex-gen-1)
  (run-test func->4 ex-gen-2))
