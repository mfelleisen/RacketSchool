#lang scribble/manual

@(require "shared.rkt")

@; -----------------------------------------------------------------------------
@title[#:tag "fri-aft"]{@bold{Robby F.} Advanced Testing} 

@author{Robby Findler}

@goals[

@item{testing languages}
@item{random testing}
@item{random testing in Redex}
]

@(define prop "property-based-testing.rkt")

Please open @link[prop]{the code} for the following exercise in a browser
and copy and paste it into your drracket. 

@exercise["robby"]{Consider the following language definition: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-language L
  (e ::=
     n
     (not e)
     (and e e)
     (or e e)
     (xor e e)
     (concat e ...)
     (+ e e))
  (n ::= (b ...))
  (b ::= 0 1))
))
@;%
 It is roughly the language of binary numbers plus some operations
 that treat them as arrays of booleans and operate elementwise
 (a conventional pun in machine architectures and low-level languages). The
 somewhat unusual @racket[concat] operation flattens out the given series
 of numbers into a single number.

Here is a reduction model of the language: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define R
  (reduction-relation
   L
   (--> (concat (b ...) ...)
        (b ... ...))))
))
@;%
It is obviously incomplete. 

By contrast, we finished the implementation: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
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
))
@;%
The metafunctions---yes, an interpreter may use metafunctions---are listed
in @figure-ref{robby-fig}.

With this interpreter, you could investigate the meaning of @tt{concat}: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(concat (1 0 1) () (1 1 0) (1 0))
))
@;%

Your @bold{task} is to finish the model of the implementation and ensure
that the two agree on as many tests as possible. Let @racket[redex-check]
be your guide; fix bugs as it discovers them to get a sense of how
property-based testing works.}

@figure["robby-fig" @list{Auxiliary functions and interpreter tests}]{
@;%
@(begin
#reader scribble/comment-reader
(racketblock0
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

(module+ test
  (check-equal? (racket-eval (term ())) 0)
  (check-equal? (racket-eval (term (1))) 1)
  (check-equal? (racket-eval (term (1 0))) 2)
  (check-equal? (racket-eval (term (1 1))) 3)
  (check-equal? (racket-eval (term (1 0 0))) 4)
  (check-equal? (racket-eval (term (1 0 1))) 5)
  (check-equal? (racket-eval (term (1 1 0))) 6)
  (check-equal? (racket-eval (term (+ (1 1 0) (1 0)))) 8)
  (check-equal? (racket-eval (term (or (1 1 0 0) (0 1 0 1))))
                (term (to-natural (1 1 0 1))))
  (check-equal? (racket-eval (term (and (1 1 0 0) (0 1 0 1))))
                (term (to-natural (0 1 0 0))))
  (check-equal? (racket-eval (term (not (1 0 1 0)))) 5)
  (check-equal? (racket-eval (term (concat (1 0 1 0) (1 1 0 0))))
                (term (to-natural (1 0 1 0 1 1 0 0))))

  (redex-check
   L natural
   (= (term (to-natural (from-natural natural)))
      (term natural)))
  
  (redex-check
   L e
   (equal? (term (redex-eval e))
           (racket-eval (term e)))))
))
@;%
}
