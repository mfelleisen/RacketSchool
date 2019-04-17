#lang racket
(require csv-reading
         racket/runtime-path
         plot/pict
         pict
         slideshow/base
         (prefix-in s: scribble/core)
         (prefix-in s: scribble/base))

(provide
 (contract-out
  [histogram (-> valid-hist-column? pict?)]
  [free-text-question (-> valid-free-text-question-columns? string?)]
  [free-text-responses (-> valid-free-text-question-columns? any/c)]))

(define hist-columns '(1 2 3))
(define (valid-hist-column? n) (member n hist-columns))

(define free-text-question-columns '(4 5))
(define (valid-free-text-question-columns? n) (member n free-text-question-columns))

(define (free-text-question column)
  (list-ref (list-ref data 0) column))

(define (free-text-responses column)
  (s:itemlist
   (for/list ([data (in-list (cdr data))]
              #:unless (regexp-match #rx"^ *$" (list-ref data column)))
     (s:item (list-ref data column)))))

(define-runtime-path survey-responses.csv "survey-responses.csv")

(define data
  (call-with-input-file survey-responses.csv
    (λ (port)
      (csv->list port))))

(define hist-range (in-range 1 6)) ;; valid responses were from 1 to 5.

(define (hist column)
  (define ht (make-hash))

  (for ([i hist-range])
    (hash-set! ht i 0))
  
  ;; cdr to drop column headings
  (define ns
    (for ([line (in-list (cdr data))])
      (define n
        (string->number (list-ref line column)))
      (hash-set! ht n (+ (hash-ref ht n 0) 1))))
  (sort
   (for/list ([(k v) (in-hash ht)])
     (vector k v))
   <
   #:key (λ (x) (vector-ref x 0))))


(define (histogram column)
  (define hist-datas
    (for/list ([i (in-list hist-columns)])
      (hist i)))
  (define biggest-number 0)
  (for ([hist-data (in-list hist-datas)])
    (for ([v (in-list hist-data)])
      (set! biggest-number
            (max biggest-number (vector-ref v 1)))))
  
  ;; round up to the nearest 10
  (set! biggest-number (* 10 (ceiling (/ biggest-number 10))))

  (for/or ([hist-data (in-list hist-datas)]
           [this-column (in-list hist-columns)])
    (and (= this-column column)
         (inset
          (vc-append (plot-pict
                      #:x-label #f
                      #:y-label #f
                      #:width 200
                      #:height 200
                      (discrete-histogram
                       hist-data
                       #:y-max biggest-number))
                     (parameterize ([current-font-size 12])
                       (para
                        #:width 200
                        (list-ref (list-ref data 0)
                                  column))))
          0 1 0 0))))

