#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "thu-mor"]{Extending Languages}

@goals[
 @item{@racket[define-syntax]}
 @item{@racket[syntax-parse]}
@; @item{@racket[define-syntax-class]}
]

@section{Functions on Syntax}

Racket's compiler is programmable. The most basic way to change the
compiler is to define a compile-time function. This is done with
@racket[define-syntax], which looks like an ordinary function definition. 

@tabular[#:sep  @hspace[5] #:style 'boxed
 @list[
  @list[ @t{regular function}         @t{compile-time function} ]
  @list[ @racket[(define (f1 x) 5)]   @racket[(define-syntax (f x) #'5)]]]]

Why @racket[#'5] instead of @racket[5]? Compile-time functions must
generate code, and the hash-quote is one way to generate code. It is just a
short-hand for @racket[syntax]. 

So how do we use @racket[f]? Like any other function, except that it is run
at compile time, not at run time. 

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(f 10)
))
@;%
Try this with plain @racket[5] as the function body. 

The other new idea is that @racket[f] takes any number of arguments: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(f)

(f 10 "hello world")
))
@;%

Weird? Let's print the input to see what's going on: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
;; Syntax -> Syntax
;; generate the same code no matter what the argument 
(define-syntax (f-display stx)
  (displayln stx)
  #'5)

(f-display)
(f-display 10)
(f-display 10 "hello world")
))
@;%
@bold{Insight} The argument of a compile-time function is the syntax tree
labeled at the root with the name of the function. 

Since Racket belongs to the Lisp family, there is a function that extracts
the underlying list from the syntax tree. We can, for example, take it is
length and translates a syntax tree into code that represents its length. 

@;%
@(begin
#reader scribble/comment-reader
(racketblock #:escape foo
(define-syntax (g stx)
  (define n (length (syntax->list stx)))
  #`#,n)
  
(g)
(g a)
(g a b)
))
@;%

Why @racket[#:escape foo #`#,n]? Well, hash-backquote also generates code. But, unlike
hash-quote it allows unquoting. And you guessed it, hash-comma is unquote
at the syntax level. 

Syntax trees come with additional properties, and we can access those. For
example, we can retrieve the line where the tree shows up or the column: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock #:escape foo
(define-syntax (i stx)
  (define l (syntax-line stx))
  (define c (syntax-column stx))
  #`(list "line and column info" #,l #,c))

(i 1)
))
@;%

And we can use Racket's list function to traverse the tree and generate a
new one: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock #:escape foo
(require (for-syntax racket/list))

(define-syntax (world stx)
  (define expr (syntax-e stx))
  (define iden (second expr))
  (define code (list 'define iden "hello world, how are you?"))
  (datum->syntax stx code))

(world hello)
hello
))
@;%
We use @racket[syntax-e] to extract the Racket list representation of the
syntax tree; extract the identifier, which we happen to know to be in
second position; create a list that looks like a definition; and translate
that list into code with @racket[datum->syntax]. 

@section{syntax-parse}

How do real functions really take apart their arguments? Pattern matching
of course! 

And @racket[syntax-parse] is yet another embedded language for pattern
matching in Racket. In contrast to @racket[match] or Redex's pattern
matcher, it is tuned to help with syntax-processing functions. 

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(require (for-syntax syntax/parse))

(define-syntax (j stx)
  (syntax-parse stx
    ((_ x) #'(define x "j"))))

(j x) 

x

(j y) 

y
))
@;%
Morally, @racket[j] is the same function as @racket[world]. But look how
much easier it is to write it down. 

And even better, imagine someone doesn't use it properly: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(j 1)
))
@;%
Now we get an error about @racket[define], even though it isn't even
visible. The developer needs to know that @racket[j] was used the wrong
way. 

We can add @defterm{annotations} to tell the pattern matcher that the
second part of the syntax tree must be an identifier or @racket[id] for
short: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (k stx)
  (syntax-parse stx
    ((_ x:id) #'(define x "k"))))

(k 1)
))
@;%
When a programmer uses @racket[k] the wrong way, the error message explains
the problem in terms of @racket[k] not the broken code it generates due to
bad syntax trees. 

We can even express this as a test: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock

(require rackunit syntax/macro-testing)

(check-exn #rx"k: expected identifier"
           (lambda () (convert-syntax-error (k 1))))
))
@;%
But let's not get carried away. 


Let's build up an ML-style let-construct: 
@itemlist[
@item{@racket[(local ((x e)) e)] and @racket[(local ((x e) (x e)) e)] are
 just @racket[let]} 
@item{@racket[(local ((x1 e1) and (x2 e2)) e)] makes @racket[x1] and
 @racket[x2] mutually recursive} 
@item{@racket[(local ((x1 e1) in (x2 e2)) e)] scopes @racket[x1] for
 @racket[e2]} 
]

We will do so one step at a time: 

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (local stx)
  (syntax-parse stx
    ((_ ((x1:id e1:expr)) e)
     #'(let ([x1 e1]) e))
    ((_ ((x1:id e1:expr) (x2:id e2:expr)) e)
     #'(let ([x1 e1][x2 e2]) e))))
))
@;%

Next we need @racket[#:literals] because we want to match @racket[and] and
nothing else: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (local stx)
  (syntax-parse stx #:literals (and)
    ((_ ((x1:id e1:expr)) e)
     #'(let ([x1 e1]) e))
    ((_ ((x1:id e1:expr) (x2:id e2:expr)) e)
     #'(let ([x1 e1][x2 e2]) e))
    ((_ ((x1:id e1:expr) and (x2:id e2:expr)) e)
     #'(letrec ([x1 e1][x2 e2]) e))))
))
@;%
Here we reuse the binding of @racket[and] from Racket because
@racket[#:literals] must use existing identifiers. 

Now @racket[in] doesn't exist in Racket. So we make a definition: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (in stx)
  (raise-syntax-error 'in "used out of context" stx))

(define-syntax (local stx)
  (syntax-parse stx #:literals (and in)
    ((_ ((x1:id e1:expr)) e)
     #'(let ([x1 e1]) e))
    ((_ ((x1:id e1:expr) (x2:id e2:expr)) e)
     #'(let ([x1 e1][x2 e2]) e))
    ((_ ((x1:id e1:expr) and (x2:id e2:expr)) e)
     #'(letrec ([x1 e1][x2 e2]) e))
    ((_ ((x1:id e1:expr) in (x2:id e2:expr)) e)
     #'(let* ([x1 e1][x2 e2]) e))))
))
@;%

@;%
@(begin
#reader scribble/comment-reader
(racketblock
(local ((x 2) (y 2)) (* x y))

(local ((x 2) in (y x)) (* x y))

(local ((f (λ (x) (g x))) and (g (λ (y) (if (= y 1) 2 (f (- y 1)))))) (f 3))
))
@;%

Let's develop the equivalent of @racket[and] without using @racket[and],
i.e., @racket[conjunction]. A simple version of @racket[conjunction] deals
with two expressions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(conjunction #true #false)
))
@;%
or 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(conjunction #true #true)
))
@;%
Only the second expression evaluates to true. At first glance, you may wish
to define @racket[conjunction] as a function that performs the usual
Boolean operation but take a look at this: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(conjunction #false (smt-solver problem-with-one-gzillion-variables))
))
@;%
We know that, even if this large-ish looking expression produces
@racket[#true], the overall expression must return @racket[#false]. Our
implementation should short-cut the evaluation. We can do this with a
compile-time function in a call-by-value language. 

So here we go: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ lhs:expr rhs:expr) #'(if lhs rhs #false)]))
))
@;%
Alternatively, 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ lhs:expr rhs:expr) #'(and-function lhs (lambda () rhs))]))

(define (and-function arg1 suspended-arg2)
  (if arg1 (suspended-arg2) #false))
))
@;%

Let's see how @racket[conjunction] could cope with multiple expressions: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(define-syntax (conjunction stx)
  (syntax-parse stx
    [(_ lhs:expr rhs:expr) #'(if lhs rhs #false)]
    [(_ lhs:expr rhs:expr rhs2:expr ...) #'(if lhs (and rhs rhs2 ...) #false)]))
))
@;%


