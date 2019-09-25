;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname duo-temp) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A flight has two pilots. Each pilot has a name and an age.
; By FAA rules, a legal flight must have two pilots 65 or under;
; design this function.

(define-struct pilot [name age])
; A Pilot is a (make-pilot String NatNum)
; Interpretation: an airline pilot!
; - name is the name of the pilot
; - age is the age of the pilot in years
; Examples:
(define PILOT-1 (make-pilot "nate" 70))
(define PILOT-2 (make-pilot "boo" 4))
(define PILOT-3 (make-pilot "tom curise" 52))

#;
(define (pilot-temp p)
  ... (pilot-name p) ... (pilot-age p) ...)

(define-struct flight [p1 p2])
; A flight is (make-flight Pilot Pilot)
; Interpretation: an airline trip
; - p1 is the captain
; - p2 is the co-pilot
; Examples:
(define FLIGHT-1 (make-flight PILOT-1 PILOT-3))
(define FLIGHT-2 (make-flight PILOT-2 PILOT-3))

#;
(define (flight-temp f)
  ... (pilot-temp (flight-p1 f))
  ... (pilot-temp (flight-p2 f)) ...)

; legal-flight? : Flight -> Boolean
; are both the pilots on the flight at most 65

(check-expect (legal-flight? FLIGHT-1) #false)
(check-expect (legal-flight? FLIGHT-2) #true)

(define (legal-flight? f)
  (and (legal-pilot? (flight-p1 f))
       (legal-pilot? (flight-p2 f))))

; legal-pilot? : Pilot -> Boolean
; is the pilot at most 65?

(check-expect (legal-pilot? PILOT-1) #false)
(check-expect (legal-pilot? PILOT-2) #true)
(check-expect (legal-pilot? PILOT-3) #true)

(define (legal-pilot? p)
  (<= (pilot-age p) 65))

(require 2htdp/image)
;;; A Silly is one of:
; - Number
; - (make-posn String Image)
; Interpretation: a silly!!
; Examples:
(define SILLY-1 6)
(define SILLY-1 (make-posn "bro" (circle 5 "solid" "red")))

#;
(define (silly-temp s)
  (cond
    [(number? s) ... s ...]
    [(posn? s) ... (posn-x s) ... (posn-y s) ...]))

(define-struct blah [ab cd])

; A foo is a (make-blah Number Bar)

(define (foo-temp f)
  ... (foo-ab f) ... (bar-temp (foo-cd f)) ...)

; A Bar is one of:
; "hello"
; - Positive Interger
; Interpretation : A bar

(define (bar-temp b)
  (cond
    [(string? b) ...]
    [(number? b) ... b ...]))






  