;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname reviewsessionnotes) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
#|
Questions

> What topics will appear on the exam?
Everything from the beginning of the semester up until (and including)
recursive data types.
Recursive data and functions for that data will certainly be on the exam.
Properly following the design recipes is worth BIG POINTS usually.
The exam will not include lists. 
There will be no big-bang specific questions.

> May use our cards?
Yes, you may bring your cards to the exam. You may also bring an 8.5x11"
study sheet (single sheet, both sides).

> Old exams have question about template for a function. What does that mean?
"Template for a function" means write a template for the main type of data
that that function processes. 
 
Remember to check your exam room assignments.
The exam is calibrated to take about an hour to complete, but we give you 3
hours to account for extra time needs, as well as the difficulty of writing
code on paper.

> Are the answers for the old exams posted on the website?
No, they are not, and will not be released at any point online.
Office hours are a great place to get the answers to old exam questions if you
are struggling with them or want to check your work. 


|#

; Exam 1, 2017
; Question 1

(define-struct answer [to the question])

(define EX1
  (make-answer "Lois" 42 (string=? "CS2500"
                                   (string-append "2500" "CS"))))
(define EX2
  (make-answer "Clark" 654 #false))

(define (check-it ans)
  (cond
    [(> (answer-the ans) 50) (answer-to ans)]
    [(not (answer-question ans)) "Greetings"]
    [else (string-append "Hello, "
                         (answer-to ans))]))

(check-it EX1)

; How to solve: Replace the
; Replace each function call (in order, starting from the inside)
; with what it evaluates to. If you end up getting the answer wrong,
; or if you are not confident, then we can look at the notes you
; made about this 'evaluation' in order to give you partial credit.
(cond
  ;[#false (answer-to EX1)]
  [#true "Greetings"]
  [else (string-append "Hello, "
                       (answer-to EX1))])
; => "Greetings"

; Looking at (answer-question EX1) separately:
(string=? "CS2500"(string-append "2500" "CS"))
(string=? "CS2500" "2500CS") ;=> #false


;; Question 2

(define-struct sling [shot])
(define-struct high [land tops])
; A Singer is one of:
; - (make-sling Boolean)
; - (make-high Number String)
; - #false
; Intepretation not needed for this problem

;; Provide three completely distinct data examples for Singer.

;; -> one for make-sling, one for make-high, one for #false

(define SINGER1 #false)
;; Please don't do it as '#false'.
;; Present it as a constant would be what is required
;; (like when doing homework)

(define SINGER2 (make-sling #true))
(define SINGER3 (make-high 25 "hello"))

;; Other phrasing: Define a representative set of examples, define a complete
;; set of examples, etc -> all mean the same thing


;; Question 3

(define-struct image-tweet [message picture])
(define-struct retweet [other-tweet])

; A Tweet is one of:    --> union data - need a cond statement
; - String
; - (make-image-tweet String Image)
; - (make-retweet String)
; Interpretation: Represents a tweet, which is either a message (String),
; a message along with a picture (make-image-tweet), or a retweet of
; another message (make-retweet)

;; Develop the template(s) for any data definition(s) you see here.

;; >> How many data definitions?
;; >> 1 or 3?

;; >> There's only one data defintion!!
;; image-tweet and retweet are struct definitions! They are not explicitly
;; data definitions on their own, but part of the data definition that is Tweet

#;(define (tweet-temp tweet)
    (cond
      [(string? tweet) ...tweet ... ] ;; already atomic
      [(image-tweet? tweet) (image-tweet-message tweet)...
                            (image-tweet-picture tweet)...]
      [(retweet? tweet) (retweet-other-tweet tweet)...]))



;; string? vs string=?
;; > (string? s) is s a string?
;; Signature: ? -> Boolean (predicate type)
;; > (string=? s1 s2) are these two strings equal?
;; Signature: String String -> Boolean


;; We only write  templates for DEFINED DATA DEFINITIONS!
;; make-image-tweet and make-retweet are not defined data definitions!!


;; Develop test cases for tweet->text, a function that takes a
;; Tweet and returns whatever text is inside it.

;; tweet->text: Tweet -> String

(define TWEET1 "CS2500")
(define TWEET2 (make-image-tweet "exam1" (empty-scene 100 100)))
(define TWEET3 (make-image-tweet "exam1-1" empty-image))
(define TWEET4 (make-retweet "more exams"))

(tweet->text TWEET1) -> "CS2500"
(tweet->text TWEET2) -> "exam1"
(tweet->text (make-retweet "more exams")) -> "more exams"

(check-expect (tweet->text TWEET3)  "exam1-1")


;; Question 4

; A PayRecord is a (make-record String Paycheck)
(define-struct record [name salary])
; Interpretation: a record of a payment to an employee

; A Paycheck is one of
; - PositiveNumber
; - (make-bonus PositiveNumber NonNegativeNumber)
(define-struct bonus [base-pay extra])

; Interpretation: A paycheck is either just some number of dollars, or
; includes both an employee's normal pay and some extra (both in dollars)


; Consider the following very badly-designed function:
; Returns the name of the employee with the greater paycheck
; PayRecord PayRecord -> String
(define (max-earner emp1 emp2)
  (if (< (record-salary emp1) (record-salary emp2))
      (record-name emp1)
      (record-name emp2)))

;; Find two bugs in this function. For each one, explain what
;; the problem is, and give a test case that does not pass
;; (i.e., either fails or crashes) to demonstrate the problem.

(max-earner (make-record "annie" 100)
             (make-record "suzanne" 150))
;; this check-expect would fail with this function because of error in comparison
;; the actual output is "annie"
;; it should have been "suzanne"

(max-earner (make-record "alice" 350)
            (make-record "brian" (make-bonus 350 100)))
;; this check-expect would fail with this function due to the error of not handling bonus
;; the actual output is "error"
;; it should have been "brian"


;; Question 5

; A DogSled is one of:
; - "sled"
; - (make-team DogSled Number)
; Interpretation: A sled pulled by a team of dogs. The numbers
; represent each dog's maximum speed.
(define-struct team [sled speed])

;; Design the function sled-speed. This function takes in a DogSled and returns a Number
;; representing the fastest the sled and team can go.
;; Assume that the sled itself can go no faster than 100 miles per hour,
;; or it would break apart, and assume that no dog can
;; run faster than its maximum speed.

;; Show all steps of the design recipe
;; ------------

; > Question: How do you know that the correct max speed for the sled is 100?
; Answer: That was the correct interpretation of the question.

;; sled-speed: DogSled -> Number
; Determine the max speed of the given DogSled.

; We will define examples separately from the tests, for clarity.
; - Empty case
; - Case where the slowest dog is first
; - Case where the slowest dog is not first
; - Case where all dogs are faster than the sled's max speed.
(define DS0 "sled")
(define DS1 (make-team DS0 10))
(define DS2 (make-team DS1 2))
(define DS3 (make-team DS2 23))

(define DS4 (make-team DS0 9000))

;; ds-temp: DogSled -> ?
#;(define (ds-temp ds)
    (cond [(string? ds) (... ds ...)]
          [(team? ds) (... (ds-temp (team-sled ds)) ;; team-sled is also a DogSled, so we have recusion!
                           (team-speed ds) ...)]))

; A DogSled that has no dogs will not move.
(sled-speed DS0) -> 100
(sled-speed DS1) -> 10
(sled-speed DS2) -> 2
(sped-speed DS3) -> 2
(sled-speed DS4) -> 100

(define (sled-speed ds)
  (cond
    [(string? ds) 100] ;; 100 is the max speed of an empty sled
    [(team? ds)
     ;; We need to return the slowest speed in this entire DogSled.
     ;; That number either comes from this team (team-speed ds)
     ;; or from within the DogSled that is inside of this current DogSled.
     ;; We can find the slowest speed in (team-sled ds) by calling
     ;; sled-speed on it!
     (min (sled-speed (team-sled ds))
          (team-speed ds))]))

; data def for reference: 
; A DogSled is one of:
; - "sled"
; - (make-team DogSled Number)



;; What are important functions to know about for the exam?
;; Boolean operators: and, or, not, if, boolean=?
;; Number operators: min, max, number?, =
;; String functions: string-append, string?, string-length, string=?




;; Bonus problem about recursive data defs:

; A PRS (PetRockStorage) is one of:
; - "King Paimon"
; - (make-bag String Number PRS)
(define-struct bag [color size contents])
; and represents the pet rock
; or a bag containing it (and possibly other bags) with a specific color and size

;; prs-temp: PRS -> ?
#;(define (prs-temp prs)
    (cond [(string? prs) (... prs ...)]
          [(bag? prs) (... (bag-color prs)
                           (bag-size prs)
                           (bag-contents prs) ...)]))

; Problem statement: Do we need more fabric to make all the bags
; for the given PRS?
; The PRS "King Paimon" needs no fabric, because it has no bags.

; bigger-than? : PRS Number -> Boolean
; Is the total size of all the bags in the PRS greater than the given size?

(define PRS0 "King Paimon")
(define PRS1 (make-bag "yellow" 10 PRS0))
(define PRS2 (make-bag "forest green" 2 PRS1))
(define PRS3 (make-bag "pink" 9 PRS2))

; Tests:
(bigger-than? PRS0 -1) -> #true ; doesn't really make logical sense, but illustrates the problem
(bigger-than? PRS0 0) -> #false
(bigger-than? PRS0 10) -> #false
(bigger-than? PRS1 20) -> #false
(bigger-than? PRS1 5) -> #true ;PRS1 has a size of 10, which is larger than 5
(bigger-than? PRS2 11) -> #true
(bigger-than? PRS3 21) -> #false ;The total size of PRS3 is exactly equal to 21
(bigger-than? PRS3 22) -> #false

(define (bigger-than? prs amount)
  ; Before we blindly follow the template for a PRS here,
  ; we should think about the task:
  ; -> Compare amount to the total size of the PRS.
  ; We should write a helper function that computes the total size of the PRS
  ; and then use the result in a greater-than comparison.
  (> (total-size prs) amount))

#|
DO NOT write this. Redundant.
  (if (> (total-size prs) amount)
      #true
      #false))
|#

; total-size : PRS -> Number
; Compute the sum of all sizes in the PRS.
(total-size PRS0) -> 0
(total-size PRS1) -> 10
(total-size PRS2) -> 12
(total-size PRS3) -> 21
(define (total-size prs)
  (cond
    [(string? prs) 0]
    [(bag? prs) (+ (bag-size prs)
                   (total-size (bag-contents prs)))]))










