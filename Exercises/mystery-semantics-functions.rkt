;; **Mystery semantics**
;; This language allows you to write `return` inside function bodies:
;; how does it behave?

(define-extended-language func-syntax-4 basic-lang
  (p ::= (prog f ... uf ... e))
  (uf ::= (defun (x x) e))
  (f ::= (%defun e (x x) e))
  (e ::= ....
     (%call x e)
     (return e)))

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
   (--> (prog (defun (x_f1 x_p1) e_1)
              (defun (x_f2 x_p2) e_2) ... e_main)
        (prog (%defun (substitute e_1 x_p1 continue) (x_f1 x_p1) e_1)
              (%defun (substitute e_2 x_p2 continue) (x_f2 x_p2) e_2) ... e_main)
        e-init)
   ;; apply
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E ((function x_fun) v_arg)))
        (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E (%call x_fun (substitute e_cont continue v_arg))))
        e-apply)
   ;; return
   (--> (prog f ... (in-hole E (%call x_fun v)))
        (prog f ... (in-hole E v))
        e-return)
   (--> (prog f_1 ... (%defun e_cont (x_fun x_param) e_body) f_2 ...
              (in-hole E_1 (%call x_fun (in-hole E_2 (return v)))))
        (prog f_1 ... (%defun (in-hole E_2 continue) (x_fun x_param) e_body) f_2 ...
              (in-hole E_1 v))
        e-return-2)))
