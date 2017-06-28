#lang racket

(require redex)
(provide (all-defined-out))


(struct test-case (test answer))

(define-syntax-rule
  (define-test id test answer)
  (define id (test-case (term test) (term answer))))

(define-syntax-rule
  (run-test lang test)
  (test-->> lang (test-case-test test) (test-case-answer test)))

(define-syntax-rule
  (run-tests lang test ...)
  (begin (run-test lang test) ...))
