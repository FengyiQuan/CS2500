;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Check & cond|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;gonna-get-an-A? : Number -> Boolean
;returns whether or not you are going to get an A given your numerical grade
;Example:
;(gonna-get-an-A? 95) => #true

(check-expect (gonna-get-an-A? 95) #true)
(check-expect (gonna-get-an-A? 45) #false)
(check-expect (gonna-get-an-A? 90) #true)

(define (gonna-get-an-A? grade)
  (>= grade 90)) ;=(or (> grade 90) (= grade 90))

;V: check-expect
;G: (check-expect funcion-call expected-value)


;V: cond
;G:
;(cond
;   [test-1 answer-1]
;   [test-2 answer-2]
;   ...
;   [test-n/else answer-n]

; sign : number -> string
; return the sign of the number

(check-expect (sign 1) "positive")
(check-expect (sign 0) "zero")
(check-expect (sign -1) "negative")

(define (sign num)
  (cond
    [(< num 0) "negative"]
    [(= num 0) "zero"]
    [(> num 0) "positive"]))

; num->grade : number -> string
; computes the letter grade based upon the number grade

(define (num->grade Num)
  (cond
   [(>= Num 90) "A"]
   [(>= Num 80) "B"]
   [(>= Num 70) "C"]
   [(>= Num 60) "D"]
   [(< Num 60) "F"]))

(check-expect (num->grade 101) "A")
(check-expect (num->grade 93) "A")
(check-expect (num->grade 90) "A")
(check-expect (num->grade 80) "B")
(check-expect (num->grade 75) "C")
(check-expect (num->grade 70) "C")
(check-expect (num->grade 62) "D")
(check-expect (num->grade 60) "D")
(check-expect (num->grade 45) "F")
(check-expect (num->grade -3) "F")