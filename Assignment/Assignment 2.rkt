;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 2
;; exercise 2
;; convert-to-inches : num num num -> num
;; computes a total number of inches by given yards, feet, and inches
(check-expect (convert-to-inches 4 1 3) 159)
(check-expect (convert-to-inches 2 2 2) 98)

(define (convert-to-inches yards feet inches)
  (+ (* yards 36) (* feet 12) inches))

;; exercise 2
;; diagon-convert-discount : num -> num
;; computes the number of Galleons  by given Pounds
(check-expect (diagon-convert-discount 13) 2)
(check-expect (diagon-convert-discount 100) 20)
(check-expect (diagon-convert-discount 1000) 200)
(check-expect (diagon-convert-discount 40) 7.4)

(define flat-fee 3)
(define exchange-rate 0.2)
(define (diagon-convert-discount Pounds)
  (cond
    [(< Pounds 100) (* (- Pounds flat-fee) exchange-rate)]
    [(>= Pounds 100) (* Pounds exchange-rate)]))

;; exercise 3
;; grayscale : num -> string
;; converts from a pixel color, supplied as an integer argument, to the visually apparent color
(check-expect (grayscale 0) "black")
(check-expect (grayscale 31) "black")
(check-expect (grayscale 32) "gray")
(check-expect (grayscale 33) "gray")
(check-expect (grayscale 239) "gray")
(check-expect (grayscale 240) "white")
(check-expect (grayscale 241) "white")
(check-expect (grayscale 255) "white")

(define (grayscale pixel)
  (cond
    [(and (>= pixel 0) (< pixel 32)) "black"]
    [(and (>= pixel 32) (< pixel 240)) "gray"]
    [(and (>= pixel 240) (<= pixel 255)) "white"]))

;; exercise 4
;; wordplay : string -> string
;; to build a word game, return certain words based on different operation
(check-expect (wordplay "first" "a" "b" "c") "a")
(check-expect (wordplay "last" "a" "b" "c") "c")
(check-expect (wordplay "hyphenate" "a" "b" "c") "a-b-c")

(define (wordplay operation word1 word2 word3)
  (cond
    [(string=? operation "first") (first (list word1 word2 word3))]
    [(string=? operation "last") (third (list word1 word2 word3))]
    [(string=? operation "hyphenate") (string-append word1 "-" word2 "-" word3)]))