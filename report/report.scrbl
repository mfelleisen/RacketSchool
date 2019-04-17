#lang scribble/base
@(require scriblib/figure
          pict
          racket/runtime-path
          "survey-responses.rkt")

@(define-runtime-path rs2018-group.jpg "rs2018-group.jpg")

@title{Report on the 2018 Racket Summer School}

In the week of July 9th 2018, we offered a five-day summer
school on language-oriented programming.

The school was supported by the NSF, industrial attendees
(who were offered an opportunity to pay a registration fee
that cost more than our expenses and thus supported others),
and SIGPLAN.

@section{Course Content & Organization}

We taught students how to design programming languages, how
to implement via compilation, how to type-check them, and
how to add conventional syntax to them, all within the
framework of Racket's language-building facilities.

Each day consisted of a morning session and an afternoon
session, with a lecture and a lab in each session. The
lecture topics were:
@itemlist[
 @item{Monday Morning: Racket and Language-oriented Programming}
 @item{Monday Afternoon: Parsing syntax, Syntax classes}
 @item{Tuesday Morning: Building a Language}
 @item{Tuesday Afternoon: Completing the Language}
 @item{Wednesday Morning: Lexing and Parsing}
 @item{Wednesday Afternoon: Building an Ugly Language}
 @item{Thursday Morning: Types and Type Checking}
 @item{Thursday Afternoon: Building a Typed Language with Macros and Turstile}
 @item{Friday Morning: Language Gems I}
 @item{Friday Afternoon: Language Gems II}]

Each lecture was about half of the time of each session and
during the remainder of the time, the students worked to
solve various exercises with hands-on help from the
organizers of the school.

@section{Attendees}

We had 75 attendees, 38 were domestic students, 7 were
foreign students, 22 were from industry, and the other 8 are
from academic institutions but not as students. Four
indicated they were female.

@section{Finances}

Each attendee cost $250 for a room in the University of Utah
dormitories for a week, $250 for breakfasts and lunches for
the week, and for attendees we supported that had to fly to
Utah, we offered $500 for transportation (some students we
supported were in driving distance and we did not offer them
any support for their travel expenses).

@section{Attendee Feedback}

@figure["fig:attendees" "Attendees of the School"]{
 @(scale (bitmap rs2018-group.jpg) .18)
}

At the end of the week, we asked attendees to evaluate the
school on a number of questions, giving scores from 1-5. The
questions and a histogram of the responses are shown in
@figure-ref["fig:histograms"].

There were also two free-form questions; the final two
sections give the students' answers.

@figure["fig:histograms" "Histograms of Numeric Question's Answers; 1 = no, 5 = yes"]{
 @(vc-append (ht-append 30
                        (histogram 1)
                        (histogram 2))
             (histogram 3))
}


@section{Responses to “@(free-text-question 4)”}

@(free-text-responses 4)

@section{Responses to “@(free-text-question 5)”}

@(free-text-responses 5)
