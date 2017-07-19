#lang racket

(provide
 ;; Any -> Void
 ;; evaluate sample-program in the Redex specification and the Records program and compare results
 ;; EFFECT if the program is a syntax erorr or the two results differ, print reasonng to STDIO 

 ;; WARNING this version does not support time-outs, exceptions, sandboxing, and other protections.
 ;; A production validation framework would need all that. 

 compare-languages-on)

;; ---------------------------------------------------------------------------------------------------
;; dependencies 

(require redex
         RacketSchool/private/mystery
         RacketSchool/private/mystery-records)

; (define RS/R1 "RacketSchool/Records1")
(define RS/R1 "RecImpl")

(module+ test
  (require rackunit)

  (define-syntax-rule
    (to-string e)
    (with-output-to-string (lambda () e))))

;; ---------------------------------------------------------------------------------------------------
;; implementation 

(define (compare-languages-on sample-program)
  (define redex-result (run record-lang-1 record->1 ,sample-program))
  (cond
    [(eq? redex-result 'syntax-error)
     (pretty-print redex-result)
     (displayln "the above program has a syntax error")]
    [else
     (define lang-result (run-lang sample-program))
     (unless (equal? lang-result redex-result)
       (pretty-print sample-program)
       (displayln "produces two different results")
       (displayln `(specification ,redex-result))
       (displayln `(implementation ,lang-result)))]))
       
;; ---------------------------------------------------------------------------------------------------
;; Records1.p -> Any or 'syntax-error or [Listof Symbol]
;; produce a value 
;; EFFECT run the sample-program (via a file) in the implementation 
(define (run-lang sample-program)
  (match (take-apart sample-program)
    [(? symbol?) 'syntax-error]
    [`(,defs ,body)
     (define file (make-temporary-file))
     (with-output-to-file file
       (lambda () (create-records1-module defs body)) #:exists 'truncate)
     (define result-as-string
       (with-output-to-string
        (lambda ()
          (with-handlers ((exn:fail? (lambda (_xn) (displayln "stuck"))))
            (dynamic-require file #f)))))
     (with-input-from-string result-as-string 
                             (lambda ()
                               (define first-value  (read))
                               (define second-value (read))
                               (cond
                                 [(not (eof-object? second-value))
                                  `(,sample-program produced two values)]
                                 [(cons? first-value)
                                  (if (eq? (second first-value) 'stuck) 'stuck first-value)]
                                 [else first-value])))]))

;; ---------------------------------------------------------------------------------------------------
;; [Listof Any] Any -> Void
;; EFFECT write a #lang Records1 program from the given defs and body to STDOUT 
(define (create-records1-module defs body)
  (display "#lang ")
  (displayln RS/R1)
  (for ((d defs)) (writeln d))
  (writeln body))

(module+ test
  (check-equal? (to-string (create-records1-module '() '(+ 1 1)))
                (string-append RS/R1 "\n" "(+ 1 1)\n"))
  (check-equal? (to-string (create-records1-module '((defun (f x) x)) '(+ 1 1)))
                (string-append RS/R1 "\n" "(defun (f x) x)\n" "(+ 1 1)\n")))
  
;; ---------------------------------------------------------------------------------------------------
;; {p from Records1} ->* [List [Listof Any] Any]
;; dissect the Records1 program into the list of definitions and the body of prog 
(define (take-apart sample-program)
  (match sample-program
    [`(prog ,d ... ,e) (list d e)]
    [else (error 'compare "the given value is not a Records1 program: ~e" sample-program)]))

(module+ test
  (check-equal? (take-apart '(prog () (+ 1 1))) '( (()) (+ 1 1) ))
  (check-equal? (take-apart '(prog ((defun (f x) x)) (+ 1 1))) '( (((defun (f x) x))) (+ 1 1) )))
