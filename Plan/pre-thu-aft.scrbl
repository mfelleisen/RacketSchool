#lang scribble/manual

@(require "shared.rkt")

@; ---------------------------------------------------------------------------------------------------
@title[#:tag "thu-aft"]{@bold{Matthew F.} Building Languages} 

@author{Matthew Flatt}

@goals[
@item{@racket[#%module-begin]}
@item{@racket[#%top-interaction]}
@item{conventional syntax}
]

On the one hand, we've looked at modeling languages in Redex. On the
other hand, we've started looking at implementing compile-time
functions as a way of defining new pieces of a language. As we'll see,
you can use comple-time functions to define a whole new language
within Racket. So, what's the relationship between Redex models and
compile-time functions?

Redex and compile-time functions reflect the two main, different ways to
implement a language in the realm of Racket. A Redex model gives you an
@deftech{interpreter}---a function that maps programs to results.
Compile-time functions can define a @deftech{compiler}---a function that
maps programs to other programs; to run the resulting program, you will
rely on the existing Racket "interpreter".  @margin-note*{The Racket
interpreter itself composes a compiler to machine code with interpretation
of that machine code.}

Whether an interepreter or a compiler is better depends on your goal.
You may well want both; you want to take a model as an interpreter and
compile programs to a call to your interpreter, which gives you some
of the benefits of both, and we'll see how to do that tomorrow
morning.

@; ----------------------------------------
@section{Extending or Defining a Language with Macros}

Up to this point, we've written @defterm{compile-time function}, but
we refine the terminology now to @defterm{macro} to reflect that we
mean a particular kind of compile-time function.

Racket macros implement @defterm{syntactic extensions}, by which we
mean that you have to learn specific rules for each macro that you
might use in a way that's qualitiatively different from having to
learn the specific behavior of each library function that you might
call. When you use a run-time function, you can know that the rest of
the program will run independent of the function as long as you don't
reach the call. More importantly, you know how argument expressons in
the function call will behave. With a macro, you don't know whether
your program will even compile if you don't know anything about the
macro (i.e., you may not have the option of running the rest of the
program), and there are no subexpressions within the macro use that
have a meaning independent of the macro.

We've seen examples all week of how you have to learn special rules
for the syntactic forms provided by Redex. Hopefully, it has also been
clear why learning and using those special rules is worthwhile to more
succinctly express program models. If you're @emph{defining} a
language, then the concern of having to specify a form's interaction
with the rest of the language is the point, anyway.

While both macros and the implementation of a conventional compiler
use @defterm{compile-time functions} (i.e., the compiler, obviously
runs at compile time), macros have the additional feature of being
able to plug into different contexts and to cooperate with other,
unknown language extensions. Along those lines, Racket macros offer a
smooth path from simple syntactic abstractions to language extensions
to whole language implementations.

To get a sense of why it's possible to implement whole new languages
with Racket macros, try running this program

@racketmod[
racket
(require (for-syntax syntax/parse))

(define-syntax (lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(cons 'x e)]))

(lambda (x) 10)
]

This example illustrates that there are no identifiers in Racket that
are special as keywords that cannot be redefined. Instead, seemingly
core parts of the language, including @racket[lambda], can be defined
as macros.

@bold{Stop}! What happens if you add @racket[(define (f x) 11)] and
@racket[(f 10)] to the program?

@exercise["ex:lambda-via-define"]{Racket's @racket[define] forms can
appear in @racket[(let () ....)] to make the definition local to the
@racket[let] form. Given that fact, define @racket[lambda] without
referring to the built-in @racket[lambda] form.}

@; ----------------------------------------
@section{Macros and Identifiers}

When we define @racket[lambda] as above, then the original
@racket[lambda] becomes inaccessible. Sometimes that's fine, but if
the intent of a new @racket[lambda] is to extend the existing
one---perhaps to add logging on each entry to the function---then we'd
like to define a new @racket[lambda] in terms of the original.

One approach is to define @racket[lambda] as the new form, but import
the original @racket[lambda] under a different name so that we can
still refer to it:

@racketmod[
racket
(require (for-syntax syntax/parse)
         (only-in racket
                  [lambda original-lambda]))

(define-syntax (lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(original-lambda (x)
         (printf "arg: ~s\n" x)
          e)]))

(define f (lambda (x) 10))
(f 2)
]

Importing the original @racket[lambda] as @racket[original-lambda]
allows the new implementation of @racket[lambda] to use it, but it
also allows the rest of the module to use @racket[original-lambda]. If
we want to write programs that only have access to the new
@racket[lambda], the best organization is to put the implementation of
@racket[lambda] in a module separate from the program that uses it.

@racketmod[#:file "noisy-lambda.rkt"
racket
(require (for-syntax syntax/parse)
         (only-in racket
                  [lambda original-lambda]))

(provide lambda)

(define-syntax (lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(original-lambda (x)
         (printf "arg: ~s\n" x)
          e)]))
]

@racketmod[#:file "program.rkt"
racket
(require "noisy-lambda.rkt")

(define f (lambda (x) 10))
(f 2)
(code:comment @#,elem{@racket[original-lambda] isn't bound here})
]

Since we may want to use the original @racket[lambda] in many ways to
implement a langauge, and since that language implementaton typically
doesn't doesn't want to use the new form directly, we usually rename
on @racket[provide] instead of on @racket[require]:

@racketmod[#:file "noisy-lambda.rkt"
racket
(require (for-syntax syntax/parse))

(provide (rename-out [new-lambda lambda]))

(define-syntax (new-lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(lambda (x)
         (printf "arg: ~s\n" x)
          e)]))
]

@exercise["ex:more-lambda"]{Add a match clause (or several) to the
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

@exercise["ex:noisy-define"]{Adjust @racket["noisy-lambda.rkt"] to
make @racket[define] create noisy functions, too, when it's used in
function-shorthand mode---like @racket[(define (f x) 11)], as opposed
to @racket[(define x 8)] or @racket[(define f (lambda (x) 11))].}

@; ----------------------------------------
@section[#:tag "whole"]{Controlling the Whole Language}

Although @racket["noisy-lambda.rkt"] provides a @racket[lambda] to
shadow the one initially provided by the @racketmodname[racket] language, we
rely on a client program to @racket[require] it within a @code{#lang
racket} without renaming the new @racket[lambda] to something else and
without requiring any other modules that provide a variant of
@racket[lambda]. To take control more reliably, we'd like a plain
@hash-lang[] line that gives the program the new @racket[lambda]
directly.

The language name listed after @hash-lang[] is almost the same as a
module name listed in @racket[require]. To extra features of
@hash-lang[] prevent us from using @racket["noisy-lambda.rkt"] after
@hash-lang[] in place of @racketmodname[racket]:

@itemlist[

 @item{A language name after @hash-lang[] is responsible not only for
 providing a set of identifier bindings, but also for declaring how to
 parse the rest of the characters after @hash-lang[], and
 @racket["noisy-lambda.rkt"] does not yet do that.}

 @item{A language name after @hash-lang[] has to be just alphanumeric
 characters plus @litchar{_} and @litchar{-}. It cannot hash quote
 marks, like @racket["noisy-lambda.rkt"].}

]

We can defer both of these constraints to an existing language
@racketmodname[s-exp], which declares that the module content is
parsed using parentheses, and that looks for a module name to provide
initial bindings (using normal Racket string syntax) right after
@racketmodname[s-exp]---but our first attempt will not work:

@racketmod[#:file "program.rkt"
s-exp "noisy-lambda.rkt"

(define f (lambda (x) 10))
(f 2)
]

The error is

@centerline{@racketerror{module: no #%module-begin binding in the module's language}}

We'll need to tell you a little more to say why the error complains
about @racket[#%module-begin], but the overall problem is that the
module after @racketmodname[s-exp] is responsible for providing
@emph{all} bindings to be used in the module body, and not just things
that differ from @racketmodname[racket]. Our example program needs, in
addition to @racket[lambda], the @racket[define] form, number
constants, function application, and module-body sequencing. Let's
define @racket["noisy-racket.rkt"] to provide our new @racket[lambda]
plus all the non-@racket[lambda] bindings of @racketmodname[racket].

@racketmod[#:file "noisy-racket.rkt"
racket
(require (for-syntax syntax/parse))

(provide (rename-out [new-lambda lambda])
         (except-out (all-from-out racket)
                     lambda))

(define-syntax (new-lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(lambda (x)
         (printf "arg: ~s\n" x)
          e)]))
]

Then we can use it as

@racketmod[#:file "program.rkt"
s-exp "noisy-racket.rkt"

(define f (lambda (x) 10))
(f 2)
]

@exercise["ex:no-divide"]{To avoid the possibility of divide-by-zero
errors, adjust @racket["noisy-racket.rkt"] to not export @racket[/],
@racket[modulo], or @racket[remainder].}

@exercise["ex:dbz-to-inf"]{To allow division without the possibility
of divide-by-zero errors, adjust @racket["noisy-racket.rkt"] export
variants of @racket[/], @racket[modulo], or @racket[remainder] that
return @racket[+inf.0] when the second argument is @racket[0].}

@; ----------------------------------------
@section{Implicit Syntactic Forms}

Triggering syntactic extensions by name allows different extensions to
be composed in a natural way, since each has its own trigger. Still,
Racket has several forms where you don't use a name. For example,
@racket[5] by itself normally treated as a literal number, instead of
requiring the programmer to write @racket[(@#,racket[quote] 5)].
Similarly, assuming that @racket[f] has a variable binding, @racket[(f
1 2)] is a function call without something before the @racket[f] to
say ``this is a function call.'' In many of these places, you might
want to extend or customize a language, even though there's no
apparent identifier to bind.

To support extension and replacement. Racket macro expander treats
several kinds of forms as having an implicit use of a particular
identifier:

@tabular[
 #:column-properties '(top top top)
 #:sep (hspace 2)
 (list
  (list @racket[5] "=" @racket[(#%datum 5)])
  '(nbsp nbsp nbsp)
  (list @racket[(f 1 2)] "=" @racket[(#%app f 1 2)])
  '(nbsp nbsp nbsp)
  (list @racketblock0[@#,hash-lang[] racket/base
                      (define (f x) x)
                      (f 5)]
        "="
        @racketblock0[(module name racket/base
                        (#%module-begin
                          (define (f x) x)
                          (f 5)))]))
]

Why does @hash-lang[] correspond to @emph{two} implicit names? Because
the @racket[module] one can't be configured. The second one,
@racket[#%module-begin], applies after the first one has imported the
@racket[#%module-begin] binding, so its meaning can be configured.

We couldn't use @racket["noisy-lambda.rkt"] as a module-language
module, because it doesn't export @racket[#%module-begin]. By
exporting everything from @racketmodname[racket] except
@racket[lambda], @racket["noisy-racket.rkt"] provides
@racket[#%module-begin], @racket[#%app], and @racket[#%datum], all of
which are used implicitly in @racket["program.rkt"].

@exercise["ex:right-to-left"]{Racket's @racket[#%app] implements
left-to-right evaluation of function-call arguments. Change
@racket["noise-racket.rkt"] so that it implements right-to-left
evaluation of arguments to a function call. You'll need to use
Racket's @racket[#%app] to implement your new @racket[#%app].}

@; ----------------------------------------
@section{Macro-Definition Shorthands}

The pattern

@racketblock[
(define-syntax (_macro-id stx)
  (syntax-parse stx 
   [(_ _pattern ....) #'_template]))
]

is common enough that it would be nice to have a shorter way of
writing it. Fortunately, we're in a language that's easy to extend
with a shorthand like @racket[define-syntax-rule], which lets you
write the above form equivalently as

@racketblock[
(define-syntax-rule
  (_macro-id _pattern ....)
  _template)
]

For historical reasons, the allowed @racket[_pattern] forms are
restricted in that they cannot include identifiers that have
@litchar{:} followed by a syntax-class name, as in @racket[x:id].
Also, the error messages are worse, so @racket[define-syntax-rule]
is normally used only for temporary or internal extensions.

There's also an intermediate point, which avoids writing an explicit
@racket[lambda] but allows multiple patterns:

@racketblock[
(define-syntax _macro-id
  (syntax-rules ()
   [(_ _pattern ....) _template]))
]

Finally, you may see @racket[syntax-case], which is almost the same as
@racket[syntax-parse], but it has the pattern-language restrictions of
@racket[define-syntax-rule] and @racket[syntax-rules]. There's little
reason to use @racket[syntax-case] over @racket[syntax-parse], other
than the minor convenience of having it included in @racketmodname[racket]
(again, for historical reasons).

@; ----------------------------------------
@section{Aside: Scope and Macro Expansion}

In Redex, @racket[define-language] lets you specify binding structure.
The @racket[define-syntax-rule] form doesn't include any such
specification. And yet...

@racketmod[
racket

(define-syntax-rule 
  (noisy-begin e ... last-e)
  (begin
   (printf "~s\n" e)
   ...
   (let ([result last-e])
     (printf "~s\n" result)
     result)))

(let ([result 1])
  (noisy-begin
   result
   2))
]

Racket's macro system can infer binding structure for macros based on
the way that macros ultimately expand. Specifically, the example macro
above expands to @racket[let], and the expander knows the binding
structure of @racket[let], so it can effectively infer a binding rule
for @racket[example]. But you know that the
@racket[define-syntax-rule] form is just a shorthand for a
compile-time functions, which can do arbitrary things... mumble mumble halting
problem mumble... so this inference is not as straightforward as, say, type
inference. In fact, the inference works dynamically (at compile time).
The details are beyond the scope (pun intended) of this summer school,
but see @hyperlink["http://www.cs.utah.edu/~mflatt/scope-sets/"]{these
notes} if you're interested.

@;{
SKIPPED

@; ----------------------------------------
@section{Aside: Phases}

When you write Racket libraries, you shouldn't use
@racket[@#,hash-lang[] racket], because @racketmodname[racket] pulls
in more than you're likely to need. Use@racket[@#,hash-lang[]
racket/base], instead, and then @racket[require] other modules that
you need.

If you follow that advice when writing macros, then you may
immediately run into a problem:

@racketmod[
racket/base
(require (for-syntax syntax/parse))

(define-syntax (lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(cons 'x e)]))

(lambda (x) 10)
]

The error is

@centerline{@racketerror{syntax: unbound identifier in the transformer environment;
 also, no #%app syntax transformer is bound}}

Although @racketmodname[racket/base] provides @racket[syntax] (as abbreviated
with @litchar{#'}) and @racket[#%app] (as implicitly used for in
function calls), it only provides then to @emph{run-time} expressions
in the module body. The @racket[syntax-parse] form is explicitly
imported for compile-time positions by @racket[(require (for-syntax
syntax/parse))]. You can similarly import @racketmodname[racket/base] for use
at compile time:

@racketmod[
racket/base
(require (for-syntax racket/base
                     syntax/parse))

(define-syntax (lambda stx)
  (syntax-parse stx
    [(_ (x:id) e:expr)
     #'(cons 'x e)]))

(lambda (x) 10)
]
}

@;{
SKIPPED

@; ----------------------------------------
@section{Context-Sensitive Expansion}

Macro expansion is context-sensitive in the sense that macro
transformations are triggered by lexical binding. The process of
matching a pattern and substituting into a template is
context-insensitive (modulo bindings for literals). To perform
context-sensitive transformations, a macro can use
@racket[syntax-local-value] to inspect its environment, and it can use
@racket[local-expand] to force the expression of a subform.
}

@; ----------------------------------------
@section{Interactive Evaluation}

When you run a program in DrRacket, you get to interact with the
program after it runs. The interactive prompt is sometimes called the
@defterm{top level}, because you have access to all the bindings that
are at the outer scope of your module, while nested bindings are
inaccessible. Interactive evaluation is similar to adding additional
definitions and expression to the end of your program---but it's not
exactly the same, because interactive evaluation cannot generally
reflect the same constraints and behaviors of in-module forms; the top
level is hopeless.

Since making interactive evaluation sensible with respect to a
module's content depends on the module's language, a
@racket[#%top-interaction] form is implicitly used for each
interaction. A replacement @racket[#%top-interaction] might disallow
definitions, or it might combine an expression's processing with
information (such as types) that is recorded from the module body.

The @racket[#%top-interaction] form is unusual in that it's paired
with its argument form using @litchar{.}, as opposed to putting
@racket[#%top-interaction] and its argument form in a syntactic list:

@racketmod[
racket

(define-syntax-rule 
  (#%top-interaction . e)
  '("So, you want to evaluate..." e "?"))
]

@exercise["ex:time-lang"]{Make a language module (to be used after
@racket[@#,hash-lang[] s-exp]) that is like @racketmodname[racket] but
adjusts @racket[#%top-interaction] to wrap @racket[time] around each
form to show how long it takes to evaluate.}

@; ----------------------------------------
@section{@hash-lang[] and Installed Languages}

We mentioned in @secref["whole"] that the language named after
@hash-lang[] must have two properties: it must take responsibility for
parsing the rest of the characters in the module, and it must be
accessible by a name that doesn't involve quote marks.

To make the module accessible without quote marks, then it needs to
reside in a directory that is registered with Racket as a
@defterm{collection}. More specifically, we normally register the
directory as a @defterm{package}, and the default treatment of a
package (unless the package says otherwise) is to use its directory as
a collection.

@margin-note{You can also use a command line by @exec{cd}ing to the
parent of the @filepath{noisy} directory and running
@commandline{raco pkg install noisy/} Don't omit the final @litchar{/},
which makes it a directory reference instead of a request to consult
the package server.}

Create a directory named @filepath{noisy} somewhere on your
filesystem. (Make the name @filepath{noisy} so that it matches our
examples.) Then choose @onscreen{Package Manager...} from DrRacket's
@onscreen{File} menu, click @onscreen{Browse...} near the top left of
the resulting window, answer @onscreen{Directory}, and pick your
@filepath{noisy} directory. Finally, click @onscreen{Install}.

Now, create a @filepath{main.rkt} file in your @filepath{noisy}. (The
name @filepath{main.rkt} is special.) Put the content of
@filepath{noisy-racket.rkt} in @filepath{main.rkt}.

It still won't work if you now try

@racketmod[
@#,racket[noisy]
]

because we've only addressed one of the problems---accessing the
module by a name without quotes. We're now ready to supply the parsing
half. Change your @filepath{main.rkt} file to add the nested
module

@racketblock[
(module reader syntax/module-reader
  noisy)
]

This declaration creates a @racket[reader] @defterm{submodule} in the
@filepath{main.rkt} module, and @racket[@#,hash-lang[] noisy] looks
for a submodule by that name in the @filepath{main.rkt} module of the
@filepath{noisy} collection.

This @racket[reader] submodule is implemented using the
language @racketmodname[syntax/module-reader], which is a language
specifically for making module parsers. The @racket[#%module-begin]
form of the @racketmodname[syntax/module-reader] module looks for a
single identifier to be injected as the language of the parsed module;
in this case, we use @racket[noisy] to refer back to the
@filepath{main.rkt} module of the @filepath{noisy} collection, which
is back to the enclosing module.

Since the @racketmodname[syntax/module-reader] language implements a
default reader that is the same as the @racketmodname[s-exp] parser,
then

@racketmod[
@#,racket[noisy]
(+ 2 3)
]

will run and print 5. It happens that

@racketmod[
s-exp noisy
(+ 2 3)
]

would run and print the same way, just using the parser via
@racketmodname[s-exp] instead of the @racket[reader] submodule.

@; ----------------------------------------
@section{@hash-lang[] and Parsing}

If the point of creating and installing @filepath{noisy/main.rkt} is
that we can use the short reference @racket[@#,hash-lang[] noisy],
then we're done. If the point is to change parsing, then we need to
override the default parser provided by @racketmodname[syntax/module-reader].

A parser comes in two flavors: @racket[read-syntax] and @racket[read].
The @racket[read] flavor is essentially legacy, but a @racket[parser]
submodule must provide it, anyway, even if just by using
@racket[read-syntax] and stripping away ``syntax'' information to get
a ``datum.'' The @racket[read] flavor takes an input stream, while the
@racket[read-syntax] flavor takes a source-file description (usually
a path) plus an input stream.

Instead of writing a parser from scratch, which can be tedious, lets
use the built-in @racket[read-syntax] and just configure it to read
decimal numbers as exact rationals instead of inexact floating-point
numbers:

@racketblock[
(module reader syntax/module-reader
  noisy
  #:read-syntax my-read-syntax
  #:read (lambda (in)
           (syntax->datum (my-read-syntax #f in)))
  
  (define (my-read-syntax src in)
    (parameterize ([read-decimal-as-inexact #f])
      (read-syntax src in))))
]

With that change, then

@racketmod[
s-exp noisy
(+ 2 3.1)
]

will show an exact result instead of a floating-point approximation.

@exercise["ex:REM"]{Some users of @racket[@#,hash-lang[] noisy] may
miss DOS-style comments using @tt{REM}. Adjust the reader so that it
detects and discards an @tt{REM} result, discarding the rest of the
line as well, and then tries reading again. Use @racket[syntax?] to
detect a non-EOF result from @racket[read-syntax], and use
@racket[read-line] to consume (the rest of) a line from an input
stream.}

@; ----------------------------------------
@section{Extended Example}

See @resource["ql.zip"]{QL}.

@; -----------------------------------------------------------------------------
@section{Resources}

@(define beautiful "http://beautifulracket.com/")

If you want to construct languages, take a look at Matthew Butterick's book
on building @link[beautiful]{Beautiful Racket}.

@(define debug "https://github.com/AlexKnauth/debug")

Matthew Butterick and Alex Knauth constructed a "meta-language"---like
s-exp and at-exp---for @link[debug]{debugging}. 

@(define lwc   "lwc-languages-the-racket-way.pdf")
@(define linde "icfp-2017-lindemayer.pdf")
@(define video "icfp-2017-video.pdf")

If you would like to read some paper on constructing DSLs, consider 
@itemlist[

@item{@link[lwc]{Dan Feltey et al.} describe how to re-create a mini version of Java,
including an IDE in the Racket world}

@item{@link[linde]{Vincent St-Amour et al.} invent and implement a language
for describing Lindemayer fractals, a paper with lots of amazing pictures,
some code, and even less text}

@item{@link[video]{Leif Andersen et al.} illustrate the language-oriented
programming idea with a small, yet reasonably complex example involving
eight embedded DSLs}
]

