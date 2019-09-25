;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 8|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require "mimic.rkt")
;; Lab 8
#|; A [Pair X Y] is a (list X Y)
(define pair? list?)
(define pair-x first)
(define pair-y second)
(define make-pair list)
 
; A [Mapping X Y] is a [List-of [Pair X Y]]
; and associates data of type X with data of type Y

; get : [Mapping X Y] X -> Y
; Get the value in the mapping associated with x-value
(check-error (get (list (make-pair 3 "three")) 4) "not found")
(check-expect (get (list (make-pair 3 "three") (make-pair 4 "four")) 4) "four")
 
; update-mapping : [Mapping X Y] X [Y -> Y] Y -> [Mapping X Y]
; Update the data associated with the x-value in the mapping using the updater function
; If the x-value is not in m, associate it with backup y-value
(check-expect (update-mapping '() 3 add1 0) (list (make-pair 3 0)))
(check-expect
 (update-mapping (list (make-pair "cat" 3.14) (make-pair "dog" 5))
                 "dog" sub1 2.5)
 (list (make-pair "cat" 3.14) (make-pair "dog" 4)))
|#

; A [Counter X] is a [Mapping X PosInt]
; and represents a multiset (a set of elements where an element can appear more than once)
 
(define MARBLE-BAG (list (make-pair "green" 2) (make-pair "red" 5)))
; MARBLE-BAG represents a bag with 2 "green" marbles and 5 "red" ones

;; Exercise (Reviewed) 1
;; add-to-counter : [Counter X] X -> [Counter X]
;; given a [Counter X] and an X will add 1 to the previously associated count
(check-expect (add-to-counter MARBLE-BAG "green")
              (list (make-pair "green" 3)
                    (make-pair "red" 5)))
(check-expect (add-to-counter MARBLE-BAG "brown")
              (list (make-pair "green" 2)
                    (make-pair "red" 5)
                    (make-pair "brown" 1)))
(define (add-to-counter counter item)
  (update-mapping counter item add1 1))

;; Exercise (Reviewed) 2
;; total-size : [Counter X] -> Number
;; grabs the total count of elements in a counter
(check-expect (total-size MARBLE-BAG) 7)
(define (total-size counter)
  (local [;; sum: [Pair X PosInt] PosInt -> PosInt
          ;; add count of pair to total-count-so-far
          (define (sum pa count-so-far)
            (+ (pair-y pa)
               count-so-far))]
    (foldr sum 0 counter)))

;; Exercise 3
;; initiate-counter : X -> [Counter X]
;; given an X creates a counter with one copy of that element
(check-expect (initiate-counter "blue") (list (make-pair "blue" 1)))
(define (initiate-counter item)
  (cons (make-pair item 1) '()))

;; Exercise 4
; expected-counts : [Counter X] Nat -> [List-of Number]
; Expected counts of elements when grabbing from the counter n times
(check-expect (expected-counts '() 100) '())
(check-expect (expected-counts MARBLE-BAG 1000)
              (list (* 2/7 1000) (* 5/7 1000)))
(define (expected-counts counter nat)
  (local [;; pair-possibility : [Mapping X Y] -> Number
          (define (pair-possibility one-item)
            (* nat (/ (pair-y one-item) (total-size counter))))]
    (map pair-possibility counter)))

;; Exercise 5
; count : [List-of X] X -> Nat
; How many times does x appear in the list?
(check-expect (count '() "b") 0)
(check-expect (count (list "a" "b" "a") "a") 2)
(define (count lox x)
  (local [(define (equa? one)
            (equal? x one))]
  (length (filter equa? lox))))

; Exercise 6
;; count-grabs : [Counter X] [List-of X] -> [List-of Number]
;; given a [Counter X] and a [List-of X], sees how many times the elements
;; from that counter appear in the list
; count-grabs : [Counter X] [List-of X] -> [List-of Number]
; See how many times the elements from this counter are in this list
(check-expect (count-grabs '() '()) '())
(check-expect (count-grabs MARBLE-BAG (list "red" "green" "red" "red")) (list 1 3))
(define (count-grabs counter lox)
  (local [(define (func pr)
            (if (equal? (pair-x pr) ]
    (map func counter)