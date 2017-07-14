#lang racket
(require redex)
(module+ test (require rackunit))

(define-language L
  (e ::=
     n
     (not e)
     (and e e)
     (concat e ...))
  (n ::= (b ...))
  (b ::= 0 1))

(define R
  (reduction-relation
   L
   (--> (concat (b ...) ...)
        (b ... ...))))

(define e? (redex-match? L e))
(define/contract (racket-eval e)
  (-> e? natural?)
  (let loop ([e e])
    (match e
      [`(+ ,e1 ,e2) (+ (loop e1) (loop e2))]
      [`(and ,e1 ,e2) (bitwise-and (loop e1) (loop e2))]
      [`(or ,e1 ,e2) (bitwise-ior (loop e1) (loop e2))]
      [`(xor ,e1 ,e2) (bitwise-xor (loop e1) (loop e2))]
      [`(not ,e)
       (term
        (to-natural
         ,(for/list ([b (in-list (term (from-natural ,(loop e))))])
            (- 1 b))))]
      [`(concat ,es ...)
       (term
        (to-natural
         ,(apply
           append
           (for/list ([e (in-list es)]
                      [i (in-naturals)])
             (term (from-natural ,(loop e)))))))]
      [`(,(? bit?) ...) (term (to-natural ,e))])))

(define-metafunction L
  to-natural : n -> natural
  [(to-natural ()) 0]
  [(to-natural (b ... 1)) ,(+ 1 (* 2 (term (to-natural (b ...)))))]
  [(to-natural (b ... 0)) ,(* 2 (term (to-natural (b ...))))])

(define-metafunction L
  from-natural : natural -> n
  [(from-natural 0) ()]
  [(from-natural natural)
   (b ... 0)
   (side-condition (even? (term natural)))
   (where (b ...) (from-natural ,(/ (term natural) 2)))]
  [(from-natural natural)
   (b ... 1)
   (where (b ...) (from-natural ,(/ (- (term natural) 1) 2)))])

(define-metafunction L
  redex-eval : e -> natural or whoops!
  [(redex-eval e)
   (to-natural n)
   (where (n) ,(apply-reduction-relation* R (term e)))]
  [(redex-eval e)
   whoops!])

(define (bit? x) (or (equal? x 0) (equal? x 1)))
  
(redex-check
 L e
 (equal? (term (redex-eval e))
         (racket-eval (term e))))
