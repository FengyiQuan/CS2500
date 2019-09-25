;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Midterm 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
;; Problem 1
(define-struct answer [to the question])
(define EX1
  (make-answer "Lois"  42  (string=? "CS2500" (string-append "2500" "CS"))))
(define EX2
  (make-answer "Clark" 654 #false))
(define (check-it ans)
  (cond
    [(> (answer-the ans) 50)     (answer-to ans)]
    [(not (answer-question ans)) "Greetings"]
    [else                        (string-append "Hello, " (answer-to ans))]))
(check-it EX1)

;; Problem 2
(define-struct sling [shot])
(define-struct high [land tops])
; A Singer is one of:
; - (make-sling Boolean)
; - (make-high Number String)
; - #false
; Intepretation not needed for this problem
;; Examples:
(define SINGER-1 (make-sling #true))
(define SINGER-2 (make-high 4 "abc"))
(define SINGER-3 #false)
#;
(define (singer-temp s)
  (cond
    [(boolean? s) ...]
    [(sling? s) ... (sling-shot s) ...]
    [(high? s) ... (high-land s) ... (high-tops s) ...]))

;; Problem 3
(define-struct image-tweet [message picture])
(define-struct retweet [other-tweet])
; A Tweet is one of:
; - String
; - (make-image-tweet String Image)
; - (make-retweet String)
; Interpretation: Represents a tweet, which is either a message (String),
; a message along with a picture (make-image-tweet), or a retweet of
; another message (make-retweet)
;; Examples:
(define TWEET-1 (make-image-tweet "howdi" (circle 2 "solid" "red")))
(define TWEET-2 (make-retweet "hello"))
(define TWEET-3 "opooos")
#;
(define (image-tweet-temp iw)
  (... (image-tweet-message iw) ...
       ... (image-tweet-picture iw) ...))
(define (retweet-temp rt)
  (... (retweet-other-tweet rt) ...))
(define (tweet-temp t)
  (cond
    [(string? t) ...]
    [(image-tweet? t) (... (image-tweet-message t) ... (image-tweet-picture t) ...)]
    [(retweet? t) (... (retweet-other-tweet t) ...)]))

;; tweet->text : Tweet -> Text
;; takes a Tweet and returns whatever text is inside it
(check-expect (tweet->text TWEET-1) "howdi")
(check-expect (tweet->text TWEET-2) "hello")
(check-expect (tweet->text TWEET-3) "opooos")
(define (tweet->text t)
  (cond
    [(string? t) t]
    [(image-tweet? t) (image-tweet-message t)]
    [(retweet? t) (retweet-other-tweet t)]))

;; Problem 4
; A PayRecord is a (make-record String Paycheck)
(define-struct record [name salary])
; Interpretation: a record of a payment to an employee

; A Paycheck is one of
; - PositiveNumber
; - (make-bonus PositiveNumber NonNegativeNumber)
(define-struct bonus [base-pay extra])
; Interpretation: A paycheck is either just some number of dollars, or
; includes both an employee's normal pay and some extra (both in dollars)
;; Examples:
(define BONUS-1 500)
(define BONUS-2 (make-bonus 500 100))
(define RECORD-1 (make-record "alice" BONUS-1))
(define RECORD-2 (make-record "bob" BONUS-2))
(define RECORD-3 (make-record "carl" BONUS-1))
;; Returns the name of the employee with the greater paycheck ;;;;; Badly-designed function
; PayRecord PayRecord -> String
(define (max-earner emp1 emp2)
  (if (< (record-salary emp1) (record-salary emp2))
      (record-name emp1)
      (record-name emp2)))

;; extract-number : Record -> Number
;; extract number of bonus
(define (extract-number emp)
  (cond [(number? (record-salary emp)) (record-salary emp)]
        [(bonus? (record-salary emp)) (+ (bonus-base-pay (record-salary emp))
                                         (bonus-extra (record-salary emp)))]))

;; max-earner1 : PayRecord PayRecord -> String
;; Returns the name of the employee with the greater paycheck
(check-expect (max-earner1 RECORD-1 RECORD-1) "tie")
(check-expect (max-earner1 RECORD-1 RECORD-2) "bob")
(check-expect (max-earner1 RECORD-2 RECORD-3) "bob")
(define (max-earner1 emp1 emp2)
  (cond [(< (extract-number emp1) (extract-number emp2)) (record-name emp2)]
        [(> (extract-number emp1) (extract-number emp2)) (record-name emp1)]
        [(= (extract-number emp1) (extract-number emp2)) "tie"]))

;; Problem 5
(define-struct team [sled speed])
; A DogSled is one of:
; - "sled"
; - (make-team DogSled Number)
; Interpretation: A sled pulled by a team of dogs.
; The numbers represent each dog's maximum speed.
;; Examples:
(define DOGSLED-1 "sled")
(define DOGSLED-2 (make-team DOGSLED-1 50))
(define DOGSLED-3 (make-team DOGSLED-2 60))
#;
(define (dogsled-temp ds)
  (cond [(string? ds) ...]
        [(team? ds) ... (dogsled-temp (team-sled ds))...
                    ... (team-speed ds) ...]))
;; sled-speed : DogSled -> Number
;; returns a Number representing the fastest the sled and team can go taking in a DogSled
(check-expect (sled-speed DOGSLED-1) 0)
(check-expect (sled-speed DOGSLED-2) 50)
(check-expect (sled-speed DOGSLED-3) 60)
(define (sled-speed ds)
  (cond [(string? ds) 0]
        [(team? ds) (if (<= (team-speed ds) 100)
                        (max (team-speed ds) (team-sled ds))
                        100)]))






