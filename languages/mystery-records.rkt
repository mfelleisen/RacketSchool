#lang racket

(require redex)
(require "basic.rkt")
(require "testing.rkt")

(provide record->1 record->2 record->3)


;; ---------------------------------------------------------------------------------------------------
;; syntax: shared

(define-extended-language record-syntax basic-lang
  (e ::= ....
     {(s e) ...}
     (e @ e))
  (v ::= ....
     {(s v) ...}))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 1: Field lookup must be on string literals

(define-extended-language record-lang-1 record-syntax
  (E ::= ....
     {(s v) ... (s E) (s e) ...}
     (E @ e)))

(define record->1
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 2: Field lookup can be on any expression that evaluates to a string

(define-extended-language record-lang-2 record-syntax
  (E ::= ....
     {(s v) ... (s E) (s e) ...}
     (E @ e)
     (v @ E)))

(define record->2
  (extend-reduction-relation basic-> record-lang-2
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; lanuage 3: Field lookup can be on any expression; it will be coerced to a string

(define-extended-language record-lang-3 record-syntax
  (E ::= ....
     {(s v) ... (s E) (s e) ...}
     (E @ e)
     (v @ E)))

(define record->3
  (extend-reduction-relation basic-> record-lang-3
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_1 ...)))))
        e-at)
   (--> (in-hole P (v @ true))
        (in-hole P (v @ "true"))
        e-coerce-true)
   (--> (in-hole P (v @ false))
        (in-hole P (v @ "false"))
        e-coerce-false)
   (--> (in-hole P (v @ n))
        (in-hole P (v @ ,(number->string (term n))))
        e-coerce-number)))


;; ---------------------------------------------------------------------------------------------------
;; language 4: Mystery semantics!
;;             How do these languages differ from language 1?
;;             Find programs that will demonstrate the difference.

(define record->4
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v)
        (side-condition (not (member (term s) (term (s_2 ...)))))
        e-at)))

(define record->5
  (extend-reduction-relation basic-> record-lang-1
   (--> (in-hole P ({(s_1 v_1) ... (s v) (s_2 v_2) ...} @ s))
        (in-hole P v)
        (side-condition (not (member (term s) (append (term (s_1 ...)) (term (s_2 ...))))))
        e-at)))


;; ---------------------------------------------------------------------------------------------------
;; tests

(define-test ex-record-1
  (prog {("x" ("a" ++ "b")) ("y" (empty? ""))})
  {("x" "ab") ("y" true)})

(define-test ex-record-2
  (prog ({("x" true) ("y" false)} @ "x"))
  true)

(define-test ex-record-3
  (prog ({("x" true) ("y" false)} @ "y"))
  false)

(define-test ex-record-4
  (prog (defun (f r) ((r @ "two") ++ (r @ "one")))
        (f {("one" "k") ("two" "o")}))
  "ok")

(define-test ex-dyn-no
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e")))
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e"))))

(define-test ex-dyn-yes
  (prog ({("one" "k") ("two" "o")} @ ("on" ++ "e")))
  "k")

(define-test ex-coerc-1-no
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  (prog ({("true" "k") ("false" "o")} @ (empty? ""))))

(define-test ex-coerc-1-kinda
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  (prog ({("true" "k") ("false" "o")} @ true)))

(define-test ex-coerc-1-yes
  (prog ({("true" "k") ("false" "o")} @ (empty? "")))
  "k")

(define-test ex-coerc-2-no
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  (prog ({("true" "k") ("false" "o")} @ (empty? "b"))))

(define-test ex-coerc-2-kinda
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  (prog ({("true" "k") ("false" "o")} @ false)))

(define-test ex-coerc-2-yes
  (prog ({("true" "k") ("false" "o")} @ (empty? "b")))
  "o")

(define-test ex-coerc-3-no
  (prog (let ((r {("7" 1) ("8" 2)})) ((r @ 7) + (r @ 8))))
  (prog (({("7" 1) ("8" 2)} @ 7) + ({("7" 1) ("8" 2)} @ 8))))

(define-test ex-coerc-3-yes
  (prog (let ((r {("7" 1) ("8" 2)})) ((r @ 7) + (r @ 8))))
  3)

(define ex-mult-fields
  (term (prog ({("a" "first") ("b" "middle") ("a" "last")} @ "a"))))


(module+ test
  (run-standard-tests record->1)
  (run-tests record->1 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->1 ex-dyn-no)
  (run-test record->1 ex-coerc-1-no)
  (run-test record->1 ex-coerc-2-no)
  (run-test record->1 ex-coerc-3-no)
  (test-->> record->1 ex-mult-fields "first"))

(module+ test
  (run-standard-tests record->2)
  (run-tests record->2 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->2 ex-dyn-yes)
  (run-test record->2 ex-coerc-1-kinda)
  (run-test record->2 ex-coerc-2-kinda)
  (run-test record->2 ex-coerc-3-no)
  (test-->> record->2 ex-mult-fields "first"))

(module+ test
  (run-standard-tests record->3)
  (run-tests record->3 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->3 ex-dyn-yes)
  (run-test record->3 ex-coerc-1-yes)
  (run-test record->3 ex-coerc-2-yes)
  (run-test record->3 ex-coerc-3-yes)
  (test-->> record->3 ex-mult-fields "first"))


;; ---------------------------------------------------------------------------------------------------
;; tests (SPOILERS!)

(module+ test
  (run-standard-tests record->4)
  (run-tests record->4 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->4 ex-dyn-no)
  (run-test record->4 ex-coerc-1-no)
  (run-test record->4 ex-coerc-2-no)
  (run-test record->4 ex-coerc-3-no)
  (test-->> record->4 ex-mult-fields "last"))

(module+ test
  (run-standard-tests record->5)
  (run-tests record->5 ex-record-1 ex-record-2 ex-record-3 ex-record-4)
  (run-test record->5 ex-dyn-no)
  (run-test record->5 ex-coerc-1-no)
  (run-test record->5 ex-coerc-2-no)
  (run-test record->5 ex-coerc-3-no)
  (test-->> record->5 ex-mult-fields
            (term (prog ({("a" "first") ("b" "middle") ("a" "last")} @ "a")))))
