#lang scribble/manual

@(require "shared.rkt")
@(require "fri-diagram.rkt")
@(define compare "compare.rkt")
@[define ryan "https://github.com/zenspider/RecImpl/blob/master/main.rkt"]

@; -----------------------------------------------------------------------------
@title[#:tag "fri-mor"]{Specification vs Implementation} 

@goals[
 @item{building a language from specification}
 @item{testing languages against specifications}
]

@section{Setting up a test bed}

Now that you have gotten far enough, let's discuss and implement
@figure-ref{model-vs-impl}.

@figure["model-vs-impl" @list{Comparing models and implementations}]{
 @centerline{@impl-vs-spec[.6]}
}

The @link[compare]{comparison module} exports a single function: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(provide
 ;; Any -> Void
 ;; evaluate sample-program in the Redex specification and the Records 
 ;; program and compare results
 ;; EFFECT if the program is a syntax erorr or the two results differ, 
 ;; print reasoning to STDIO 

 ;; WARNING this version does not support time-outs, exceptions, sandboxing, 
 ;; and other protections. A production validation framework would need all that. 

 compare-languages-on)
))
@;%
As the comments say, @racket[compare-languages-on] makes it convenient to
compare two versions of the same language: its model (blueprint) and its
realization (implementation). 

Using this function, we can set up a bunch of comparisons like this: 
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(compare-languages-on '(prog (++ "a" "n")))

(compare-languages-on
 '(prog (defun (f x) (record ("a" x)))
        (|@| (f 1) "a")))

(compare-languages-on
 '(prog (defun (f x)
          (let ((r (record ("aa" 00) ("c" x))))
            r))

        (defun (g r)
          (if (zero? (|@| r "aa"))
              (|@| r "c")
              (|@| r "aa")))

        (defun (h some-f)
          (some-f 1))

        (g (h f))))

(compare-languages-on '(prog (+ 1 "a")))

;; why is the following an unreasonable test? 
;; (compare-languages-on '(prog (function f)))
@; Because it tests free variableness instead of something useful

(compare-languages-on '(prog (defun (f x) x) f))

(compare-languages-on
 '(prog +))
))
@;%
Of course, in the end we want many more comparisons than five, and how to
 set this up is the topic of the concluding lecture in the afternoon. 

@bold{Note} Ryan Davis and Sergej Koscejev volunteered to subject their
 @link[ryan]{implementation} of @tt{Records1} to the above testing. In the
 meantime, they have implemented their own comparison function and improved
 their implementation to pass a 1,000 tests.
