;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname abstraction) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; design the function add-7-to-all that
; accepts a [List-of Number] and adds 7 to every number
; add-7-to-all : [List-of Number] -> [List-of Number]
(check-expect (add-7-to-all '()) '())
(check-expect (add-7-to-all (list 1 2 3)) (list 8 9 10))
(define (add-7-to-all lon)
  (map add-7 lon))
; add-7 : Number -> Number
; adds 7 to a number
(check-expect (add-7 0) 7)
(check-expect (add-7 4) 11)
(define (add-7 n)
  (+ n 7))

; design the function only-negatives that
; accepts a [List-of Number] and keeps only the negative numbers
; only-negatives : [List-of Number] -> [List-of Number]
(check-expect (only-negatives '()) '())
(check-expect (only-negatives (list 1 2 3)) '())
(check-expect (only-negatives (list -1 -2 -3)) (list -1 -2 -3))
(define (only-negatives lon)
  (filter negative? lon))