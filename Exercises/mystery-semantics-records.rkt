;; **Mystery semantics**
;; How do these languages differ from Records1?
;; Find programs that will demonstrate the difference.

(define record->4
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_2 ...)))))
        e-at)))

(define record->5
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P (@ (record (s_1 v_1) ... (s v) (s_2 v_2) ...) s))
        (in-hole P v)
        (side-condition (not (member (term s) (append (term (s_1 ...)) (term (s_2 ...))))))
        e-at)))
