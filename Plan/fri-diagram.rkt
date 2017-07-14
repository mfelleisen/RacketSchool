#lang racket

(provide impl-vs-spec)

(provide impl-vs-spec)

(require slideshow)

;; String -> Pict 
(define (language s)
  (define s:pict (t s))
  ; (displayln `(,(pict-width s:pict) ,(pict-height s:pict)))
  (define frame (rounded-rectangle 300 #;(+ (pict-width s:pict) 10) (+ (pict-height s:pict) 10)))
  (cc-superimpose frame s:pict))

(define impl (language "implementation of L"))
(define spec (language "specification of L"))

(define (test-case s)
  (define case  (t s))
  (define frame (ellipse (+ (pict-width case) 30) (+ (pict-height case) 20)))
  (cc-superimpose case frame))

(define ins (test-case "valid program in L"))

(define i-out (test-case "impl.'s output"))
(define s-out (test-case "spec.'s output"))

(define comparison (language "comparison"))

(define (impl-vs-spec x)
  (let* ([s 
          (vc-append
           100
           ins
           (hc-append 50 (vc-append 50 spec s-out) (vc-append 50 impl i-out))
           comparison)]
         [s (pin-arrow-line 5 s ins cb-find impl ct-find
                            #:start-angle (* 3/2 pi)
                            #:start-pull .1
                            #:end-angle (* 3/2 pi)
                            #:end-pull .2)]
         [s (pin-arrow-line 5 s ins cb-find spec ct-find
                            #:start-angle (* 3/2 pi)
                            #:start-pull .1
                            #:end-angle (* 3/2 pi)
                            #:end-pull .2)]
         [s (pin-arrow-line 5 s spec cb-find s-out ct-find)]
         [s (pin-arrow-line 5 s impl cb-find i-out ct-find)]
         [s (pin-arrow-line 5 s s-out cb-find comparison ct-find
                            #:start-angle (* 3/2 pi)
                            #:start-pull .1
                            #:end-angle (* 3/2 pi)
                            #:end-pull .2)]
         [s (pin-arrow-line 5 s i-out cb-find comparison ct-find
                            #:start-angle (* 3/2 pi)
                            #:start-pull .1
                            #:end-angle (* 3/2 pi)
                            #:end-pull .2)])
         
    (scale s x)))
