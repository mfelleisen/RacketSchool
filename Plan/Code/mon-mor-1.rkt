#lang racket

(require redex)

;; -----------------------------------------------------------------------------
(define-language Lambda1
  (e ::=
     x
     (lambda (x) e)
     (e e))
  (x ::= variable-not-otherwise-mentioned))

(define e1 (term (lambda (x) y)))
(define e2 (term (lambda (z) y)))

(default-language Lambda1)
(term (substitute ,e1 y x))
(term (substitute ,e2 y x))

;; -----------------------------------------------------------------------------
(define-language Lambda
  (e ::=
     x
     (lambda (x) e)
     (e e))
  (x ::= variable-not-otherwise-mentioned)
  #:binding-forms
  (lambda (x) e #:refers-to x))

(default-language Lambda)
(term (substitute ,e1 y x))
(term (substitute ,e2 y x))