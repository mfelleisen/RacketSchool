#lang racket

(require redex)
(require test-engine/racket-tests)
(require "mystery-functions.rkt")
(require "mystery-variables.rkt")
(require "mystery-records.rkt")

(provide
 run
 stuck?
 (rename-out
  [record->1 records1]
  [record->2 records2]
  [record->3 records3]
  [func->1 functions1]
  [func->2 functions2]
  [func->3 functions3]
  [var->1 variables1]
  [var->2 variables2]
  [var->3 variables3]))

(define-syntax-rule (run lang e)
  (car (apply-reduction-relation* lang (term e))))

(define (stuck? e)
  (and (pair? e) (eq? (car e) 'prog)))