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
from academic institutions but not as students.

We asked applicants to indicate if they were from an
underrepresented group. Using their own labels, four
indicated they were female, three black, two hispanic, one
gay male, one gender non-conforming, one non-binary, one
LGBT, and four from areas in the world that are not well
represented (Nepal, Puerto Rico, Brazil, and an unspecified
developing country). All of those attendees attended the
school.

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


Every evening during the week, we asked the attendees to respond to some
basic questions about the day. For example, we asked whether they had
understood the lecture material, whether they could work with it, whether
they thought they could already apply the ideas. We used this formative
evaluation to adjust the lectures of the next day or, in one extreme case,
to re-arrange the weekly schedule. 

At the end of the week, we ran a summative evaluation about the school. 
The evaluation allowed for both ranked answers as well as free-form
responses. The goal of the evaluation was to find out how well the
presented material matched the advertisement, how well the students
learned, and whether all of this matched their expectations. @Figure-ref["fig:histograms"]
summarizes the attendee's answer with bar charts. These charts demonstrate
that the vast majority of attendees agreed that the material matched and
that they understood it at the expected level.

The following appendix collects the raw source material from the free-form
responses. They are reflective of the summary. Many responses express
a high degree of satisfaction with the summer school though some also
offer criticism.

@figure["fig:histograms" "Histograms of Numeric Question's Answers; 1 = no, 5 = yes"]{
 @(vc-append (ht-append 30
                        (histogram 1)
                        (histogram 2))
             (histogram 3))
}

@section{Our Response}

We have used the feedback from last year's summer school to adjust the
school for the 2019 incarnation. The most important change is to separate
Matthew Butterick's Beautiful Racket material from the academic
one. Beautiful Racket requires less knowledge about Racket than the academic summer
school and is thus suitable for Racket beginners. 

For the academic summer school we have stretched the schedule. It now comes
with a significantly slower introduction to syntactic abstractions than
last year's. Instead of a single day, we will use a day and a half. We are
prefixing the schedule with an overview of Language-Oriented Programming
and we are concluding with a research overview. 

Finally, we are rotating in Jay McCarthy as a lecturer on macros to replace
Matthias Felleisen, and we have invited Jesse Tov from Northwestern to take
over for Stephen Chang to teach the material on typed domain-specific
languages. 

@(require (only-in scribble/core element style))
@(element (style "appendix" '()) '())

@section{Responses to “@(free-text-question 4)”}

@(free-text-responses 4)

@section{Responses to “@(free-text-question 5)”}

@(free-text-responses 5)
