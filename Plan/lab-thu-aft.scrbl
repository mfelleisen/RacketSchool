#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "lab-thu-aft"]{@bold{Lab} Practice with Hash Langs}


@section[#:style 'unnumbered]{Minor Exercises}

This section collects the exercises from @secref{thu-aft}. 

@exercise["ex:lambda-via-define-v2"]{Racket's @racket[define] forms can
appear in @racket[(let () ....)] to make the definition local to the
@racket[let] form. Given that fact, define @racket[lambda] without
referring to the built-in @racket[lambda] form.}

@exercise["ex:more-lambda-v2"]{Add a match clause (or several) to the
@racket[new-lambda] macro so that @racket[lambda] shapes
(trees) other than  
@;%
@(begin
#reader scribble/comment-reader
(racketblock
(lambda (x:id) e:expr)
))
@;%
behave as before. @bold{Note} If you know more than basic Racket but not
the whole language, just get some shapes to work---not all of
@racket[lambda].}

@exercise["ex:noisy-define-v2"]{Adjust @racket["noisy-lambda.rkt"] to
make @racket[define] create noisy functions, too, when it's used in
function-shorthand mode---like @racket[(define (f x) 11)], as opposed
to @racket[(define x 8)] or @racket[(define f (lambda (x) 11))].}

@exercise["ex:no-divide-v2"]{To avoid the possibility of divide-by-zero
errors, adjust @racket["noisy-racket.rkt"] to not export @racket[/],
@racket[modulo], or @racket[remainder].}

@exercise["ex:dbz-to-inf-v2"]{To allow division without the possibility
of divide-by-zero errors, adjust @racket["noisy-racket.rkt"] export
variants of @racket[/], @racket[modulo], or @racket[remainder] that
return @racket[+inf.0] when the second argument is @racket[0].}

@exercise["ex:right-to-left-v2"]{Racket's @racket[#%app] implements
left-to-right evaluation of function-call arguments. Change
@racket["noise-racket.rkt"] so that it implements right-to-left
evaluation of arguments to a function call. You'll need to use
Racket's @racket[#%app] to implement your new @racket[#%app].}
       
@exercise["ex:time-lang-v2"]{Make a language module (to be used after
@racket[@#,hash-lang[] s-exp]) that is like @racketmodname[racket] but
adjusts @racket[#%top-interaction] to wrap @racket[time] around each
form to show how long it takes to evaluate.}

@exercise["ex:REM-v2"]{Some users of @racket[@#,hash-lang[] noisy] may
miss DOS-style comments using @tt{REM}. Adjust the reader so that it
detects and discards an @tt{REM} result, discarding the rest of the
line as well, and then tries reading again. Use @racket[syntax?] to
detect a non-EOF result from @racket[read-syntax], and use
@racket[read-line] to consume (the rest of) a line from an input
stream.}

@section[#:style 'unnumbered]{Major Exercises}

Start with the @resource["ql.zip"]{QL} implementation. You can pick any of
the QL variants as a starting point, but one with at least type
checking will be the most interesting, and the one with
non-S-expression syntax should be within reach.

@exercise["ex:ql:if"]{Add an @racket[if] form for use in guards or
    expressions to compute field values.}

@exercise["ex:ql:text"]{Add a @racket[text] field type, where
    @filepath{gui.rkt} already provides @racket[text-widget].}
