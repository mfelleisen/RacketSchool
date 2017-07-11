;; **Mystery semantics**
;; How does this language differ from Variables1?
;; Find a program that will demonstrate the difference.

(define var->4
  (extend-reduction-relation var->1 var-lang
   ;; let
   ;; (This is like the `let` rule in Variables1, except that it does not create a fresh name.)
   (--> (prog f ...
              (in-hole E (let ((x v)) e)))
        (prog f ... (defvar x v)
              (in-hole E e))
        e-let2)))

;; For reference: here is the let rule used by Variables1
(define var->
  (extend-reduction-relation basic-> var-lang
   (--> (prog f ...
              (in-hole E (let ((x v)) e)))
        (prog f ... (defvar x_fresh v)
              (in-hole E (substitute e x x_fresh)))
        (fresh x_fresh)
        e-let)))
