;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname temp2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;you are running a car dealership
;the dealership sells 4 ytpes of cars : coupes, hatchbacks, sedans, and minivans
;design the function num-doors that returns the number of doors each car has.

;A Car is one of:
; - "coupe"
; - "hatchback"
; - "sedan"
; - "minivan"
; interpretation: a type of car sold by this dealership
;examples:
(define CAR-COUPE "coupe")
(define CAR-HATCHBACK "hatchback")
(define CAR-SEDAN "sedan")
(define CAR-MINIVAN "minivan")

(define (car-temp c)
  (cond
  [(string=? c CAR-COUPE) ...]
  [(string=? c CAR-HATCHBACK) ...]
  [(string=? c CAR-SEDAN) ...]
  [(string=? c CAR-MINIVAN) ...]))


; num-doors :car -> PositiveInterger
; takes a car type and returns the nubmer of doors

(check-expect (num-doors CAR-SEDAN) 4)
(check-expect (num-doors CAR-COUPE) 2)
(check-expect (num-doors CAR-HATCHBACK) 5)
(check-expect (num-doors CAR-MINIVAN) 5)

(define (num-doors c)
  (cond
  [(string=? c CAR-COUPE) 2]
  [(string=? c CAR-HATCHBACK) 5]
  [(string=? c CAR-SEDAN) 4]
  [(string=? c CAR-MINIVAN "minivan") 5])); copy paste