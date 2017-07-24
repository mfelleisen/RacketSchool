#lang scribble/base
@(require scribble/core
          scribble/html-properties
          net/url-string)

@(define (section* #:tag [tag #f] . c) (apply section #:style 'unnumbered #:tag tag c))

@(define (during-school . l)
   (if #f
       @elem[#:style "duringSchool" l]
       null))

@(define (after-school . l)
   (if #t
       @elem[#:style "afterSchool" l]
       null))

@(define page-style
   (make-style
    #f
    (list
     'no-sidebar
      ;; Redundant if 'no-sidebar is included, but makes a reasonable TOC otherwise:
      'no-toc 'toc-hidden
      (make-css-addition (string->url "https://fonts.googleapis.com/css?family=Martel|Open+Sans"))
      (make-css-addition #"body { background: #DDD;
                                  background-image: url(http://racket-lang.org/img/racket-logo.svg);
                                  background-repeat: no-repeat; 
                                  background-size: 10em 10em; 
                                  background-position: 1em 1em; 
                                  font-size: 12pt; }
                           .maincolumn { background: white; padding: 2ex; 
                                         margin-top: -2ex; width: 45em; margin-left: 12em; }
                           .main, .refcontent, .tocview, .tocsub, .sroman, i { font-family: 'Martel', serif; }
                           .refcolumn { background: none; border: 0; }
                           .duringSchool, .afterSchool { color: #B00; }
                           h1, h2, h3 { font-family: 'Open Sans', sans-serif; }
                           h3 { border-top: 1px solid #888; }"))))

@title[#:style page-style]{@larger{The Racket School of Semantics and Languages}
                           @linebreak[] July 10-14, 2017 @hspace[4] Salt Lake City, Utah, USA}

@margin-note{@(tabular
               (list (list @italic{@nonbreaking{Thanks to our generous supporters:}})
                     (list @larger{@nonbreaking{National Science Foundation}})
                     (list @larger{SIGPLAN})
                     (list @elem{Jane Street})
                     (list @elem{Microsoft})
                     (list @elem{David Ventimiglia})
                     (list @elem{Jim Sandridge})))}


@during-school{@bold{New}: Local details for participants highlighted below.}

@after-school{@bold{New}: Here the the @hyperlink["notes/index.html"]{notes} for the summer school.}

@section*{Overview and Audience}

The
@hyperlink["http://racket-lang.org/"]{Racket}
team has spent over thirty years developing and refining a coherent intellectual tradition for studying and building programming languages. The Racket Summer School will introduce participants to this framework with lectures and hands-on exercises, focusing on the transition from working programming languages to semantics and back. Concretely, it will cover the following topics:
@itemlist[

@item{Reduction semantics with evaluation contexts.}

@item{The Redex languages for describing and exploring such a semantics.}

@item{The @tt{#lang} mechanism for defining languages and environments.}

@item{Semantics (re)engineering.}

@item{Connections between all of the above.}

]

Ideal attendees are
@itemlist[

@item{Current young PhD students who want training in a systematic approach to semantics and languages.}

@item{Senior PhD students or post-docs who are already versed in other approaches and want to expose themselves to the alternative Racket philosophy.}

@item{Senior undergraduates and master's students who wish to prepare themselves for a PhD program.}

@item{Industrial developers looking to broaden and deepen their toolkit.}

@item{Computing professionals who want a preview of research results that will impact computing in coming years.}

@item{Faculty who wish to experience different and new approaches to teaching programming languages.}

]
If you don't fit one of these categories but are still interested, go ahead and tell us about yourself
[@secref["application"]]!

@section*{Dates and Location}

The School will run July 10-14 (Monday-Friday) at the
@hyperlink["http://www.cs.utah.edu/"]{University of Utah}.

The University is located in lovely
@hyperlink["https://www.visitsaltlake.com/"]{Salt Lake City}, Utah, USA.

Utah is home to several US National Parks, Monuments, and Sites, the Sundance Film Festival, the Bonneville Salt Flats, and much more. Learn more
@hyperlink["https://travel.utah.gov/"]{here},
@hyperlink["https://utah.com/"]{here},
or
@hyperlink["https://www.visitutah.com/"]{here}.

@during-school{The summer school will be held in room 2230 of the
@hyperlink["https://goo.gl/maps/bpWGz93472s"]{Warnock Engineering Building (WEB)}.}

@during-school{To get to campus, one option is
@hyperlink["https://www.rideuta.com/Services/TRAX"]{TRAX light rail}
to either the @hyperlink["https://goo.gl/maps/UnTF4ZSykBn"]{Fort
Douglas} or Stadium stop. Walk to WEB from there, or a
@hyperlink["http://www.uofubus.com/"]{free campus shuttle} stops at
TRAX stations and near WEB.}

@section*{Faculty}

The School will be taught by
@hyperlink["http://www.ccs.neu.edu/home/matthias/"]{Matthias Felleisen},
@hyperlink["https://www.eecs.northwestern.edu/~robby/"]{Robby Findler},
@hyperlink["http://www.cs.utah.edu/~mflatt/"]{Matthew Flatt},
@hyperlink["https://cs.brown.edu/~sk/"]{Shriram Krishnamurthi},
@hyperlink["https://jeapostrophe.github.io/home/"]{Jay McCarthy}, and
@hyperlink["http://justinpombrio.net/"]{Justin Pombrio}.


@section*{Week Schedule}

The workshop will run roughly 9am to 5pm Monday through Friday, with each day divided into four sections of about 1h15m each:

@(define cont @italic{cont'd})

@tabular[#:sep @hspace[4] #:style 'boxed #:row-properties '(bottom-border ()) #:column-properties '((right-border right) ())
@(list

    @list[""                @bold{AM 1}                   @bold{AM 2}       @bold{PM 1}               @bold{PM 2}]

    @list[@bold{Monday@'nbsp}     "Operational semantics"   "Redex"               "Operational semantics"   "Redex"]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Tuesday@'nbsp}    "Mystery 1: discovering"  "Mystery 1: modeling" "Mystery 2: discovering"  "Mystery 2: modeling"]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Wednesday@'nbsp}  "Mystery 3: discovering"  "Mystery 3: modeling" "Semantics reengineering" @italic{break}]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Thursday@'nbsp}   @tt{define-syntax}        (para @tt{#lang} ", " @tt{#%app})     "building your own language and IDE" 'cont]

  @list["" 'cont 'cont 'cont 'cont]

    @list[@bold{Friday@'nbsp}     (para @tt{#lang} " from Redex")  "Generate tests"      "Test your language" 'cont]

)
]

@during-school{Breakfast (bagels and fruit) will be available by 8:30
each day, and lunch will be delivered.}

@section*{Accommodation}

We have arranged for subsidized lodging in dorms at the University of Utah.

@; @section*{Parking}

@during-school{Participants can park in the Merrill Engineering
Building (MEB) lot to the north of WEB and MEB. Contact the organizers
for a virtual parking pass.}

@section*{Costs and Financial Support}

There is no cost for attending the workshop itself.

Funding is provided by US National Science Foundation to support participants from US institutions.
This support will provide full accommodation in the dorm and reasonable food and travel allowances.

People who are not eligible for funding are also welcome.

@section*[#:tag "application"]{Application}

Please express interest
@hyperlink["https://docs.google.com/forms/d/e/1FAIpQLSezqGvqMWgtHuH7kWhGDU6uu8N-pSIDqdxEmwcj20lEurtnhQ/viewform#responses"]{through this form}.
We will contact people directly.