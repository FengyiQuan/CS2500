;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname make-posn) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;V: make-posn
;G: (make-posn x-value y-value)
;S: you now have a thing with an x-value and a y-value

;V: posn-x/y
;G: (posn-x (make-posn x-value y-value))
;   (posn-y (make-posn x-value y-value))
;S: get x/y out of the (make-posn)

;; A position is a (make-posn num num)
;; interpretation: x/y coordinates
(define POSITION-1-2 (make-posn 1 2))
(define POSITION-2-1 (make-posn 2 1))
(define POSITION-0-0 (make-posn 0 0))

(define (position-temp p) ;template
  (...(posn-x p)... (posn-y p)))

; x-greater-than-y? : Position -> Boolean
;returns true is the x coordinate is greater than the y coordinate
(check-expect (x-greater-than-y? POSITION-1-2) #false)
(check-expect (x-greater-than-y? POSITION-2-1) #true)
(check-expect (x-greater-than-y? POSITION-0-0) #false)

(define (x-greater-than-y? p)
  (cond
    [(> (posn-x p) (posn-y p)) #true]
    [(< (posn-x p) (posn-y p)) #false]
    [(= (posn-x p) (posn-y p)) #false]))


;add-10-to-x : position ->position
;returns a new position with 10 added to the x-coordinate
(check-expect (add-10-to-x POSITION-1-2) (make-posn 11 2))
(check-expect (add-10-to-x POSITION-2-1) (make-posn 12 1))
(check-expect (add-10-to-x POSITION-0-0) (make-posn 10 0))

(define (add-10-to-x p)
  (make-posn (+ 10 (posn-x p)) (posn-y p)))