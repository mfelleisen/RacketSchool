#lang racket

(provide (rename-out [records-top #%top-interaction]
                     [records-module #%module-begin]))

;; ---------------------------------------------------------------------------------------------------

(require "lambda-with-records.rkt"
         (for-syntax syntax/parse)
         redex/reduction-semantics)

(define-syntax (records-top stx)
  (syntax-parse stx
    [(_ . e) #'(#%top-interaction . (eval-via-steps (term e)))]))
 
(define-syntax (records-module stx)
  (syntax-parse stx
    [(_ e ...) #'(#%module-begin (apply values (append (eval-via-steps (term e)) ...)))]))

(define (eval-via-steps e)
  (apply-reduction-relation* ->records e))