#lang racket

(require (for-syntax syntax/parse))
(require (for-syntax racket/list))

;; Syntax -> Syntax
;; generate the same code no matter what the argument 
(define-syntax (f stx)
  (displayln stx)
  #'5)

(f)
(f 10)
(f 10 "hello world")

;; add displayln
;; insight: stx contains a list 

;; Syntax -> Syntax
;; generate code that represents the length of the syntactic form 
(define-syntax (g stx)
  (define n (length (syntax->list stx)))
  #`#,n)
  
(g)
(g a)
(g a b)

;; Syntax -> Syntax
;; traverse syntax and generate code for various cases 
(define-syntax (h stx)
  (displayln stx)
  (define t (syntax->list stx))
  (define s (syntax-e (second t)))
  #`#,s)

(h "a")

;; insight: syntax-e delivers the list of syntax-es
;; insight: there is more to syntax than lists and symbols

(define-syntax (i stx)
  (define l (syntax-line stx))
  (define c (syntax-column stx))
  #`(list "line and column info" #,l #,c))

(i 1)

(define-syntax (constant stx)
  (define expr (syntax-e stx))
  (define iden (second expr))
  (define code (list 'define iden "constant"))
  #`#,code)

(constant hello)
hello


;; how do functions really take apart their arguments? pattern matching

;; syntax-parse: yet another embedded language for pattern matching,
;; especially designed for dealing with syntax

(define-syntax (j stx)
  (syntax-parse stx
    ((_ x) #'(define x "j"))))

(j x) x
(j y) y
; (j 1)

(define-syntax (k stx)
  (syntax-parse stx
    ((_ x:id) #'(define x "k"))))

; (k 1)

(require rackunit syntax/macro-testing)

(check-exn (lambda (x) (string=? "k: expected identifier" (exn-message x)))
           ; #rx"k: expected identifier"
           (lambda () (convert-syntax-error (k 1))))
