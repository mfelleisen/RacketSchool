#lang racket

(require "mystery.rkt")
(require test-engine/racket-tests)

;; just testing that mystery languages can be run

(check-expect
 (run functions1
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (x + 1))
            (twice inc)))
 3)

(check-expect
 (run functions2
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (x + 1))
            (twice inc)))
 3)

(check-expect
 (run functions3
      (prog (defun (twice f) (f (f 1)))
            (defun (inc x) (x + 1))
            (twice inc)))
 2)

(check-satisfied
  (run records1
       (prog (let ((r {("doghouse" 1)}))
               (r @ ("dog" ++ "house")))))
  stuck?)

(check-expect
 (run records2
      (prog (let ((r {("doghouse" 1)}))
              (r @ ("dog" ++ "house")))))
 1)

(check-satisfied
 (run records2
      (prog (let ((r {("true" 1)})) (r @ true))))
 stuck?)

(check-expect
 (run records3
      (prog (let ((r {("true" 1)})) (r @ true))))
 1)

(check-expect
 (run variables1
      (prog (defun (f x) (set! x (x + 1))) (let ((x 1)) (begin (f x) x))))
 1)

(check-expect
 (run variables2
      (prog (defun (f x) (set! x (x + 1))) (let ((x 1)) (begin (f x) x))))
 2)

(check-expect
 (run variables3
      (prog (defun (f x) (set! x (x + 1))) (let ((x 1)) (begin (f x) x))))
 2)

(test)