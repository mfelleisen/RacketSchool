#lang racket

(require "compare.rkt")

(compare-languages-on '(prog (++ "a" "n")))

(compare-languages-on
 '(prog (defun (f x) (record ("a" x)))
        (@ (f 1) "a")))

(compare-languages-on
 '(prog (defun (f x)
          (let ((r (record ("aa" 00) ("c" x))))
            r))

        (defun (g r)
          (if (zero? (@ r "aa"))
              (@ r "c")
              (@ r "aa")))

        (defun (h some-f)
          (some-f 1))

        (g (h f))))

(compare-languages-on '(prog (+ 1 "a")))

; (compare-languages-on '(prog (function f)))

(compare-languages-on '(prog (defun (f x) x) f))

(compare-languages-on
 '(prog +))