;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample02) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 1
(check-expect (convert-to-inches 1 1 1) 49)
(check-expect (convert-to-inches 4 1 3) 159)

(define (convert-to-inches y f i)
  (+ (* y 36) (* f 12) i))

;; Exercise 2
(check-expect (diagon-convert-discount 13) 2)
(check-expect (diagon-convert-discount 50) 9.4)
(check-expect (diagon-convert-discount 100) 20)
(check-expect (diagon-convert-discount 500) 100)

;; diagon-convert-discount : Number -> Number
;; Convert from pounds to galleons
(define (diagon-convert-discount pounds)
  (cond
    [(< pounds 100) (/ (- pounds 3) 5)]
    [else (/ pounds 5)]))

;; Exercise 3
(check-expect (grayscale 0) "black")
(check-expect (grayscale 30) "black")
(check-expect (grayscale 31) "black")
(check-expect (grayscale 32) "gray")
(check-expect (grayscale 33) "gray")
(check-expect (grayscale 34) "gray")
(check-expect (grayscale 239) "gray")
(check-expect (grayscale 240) "white")
(check-expect (grayscale 241) "white")
(check-expect (grayscale 242) "white")
(check-expect (grayscale 255) "white")

;; grayscale : Number -> String
;; Produces the visually apparent color given a pixel color
(define (grayscale v)
  (cond
    [(< v 32) "black"]
    [(>= v 240) "white"]
    [else "gray"]))

;; Exercise 4
(check-expect (wordplay "first" "a" "b" "c") "a")
(check-expect (wordplay "last" "a" "b" "c") "c")
(check-expect (wordplay "hyphenate" "a" "b" "c") "a-b-c")
(check-expect (wordplay "first" "cat" "dog" "mouse") "cat")
(check-expect (wordplay "last" "cat" "dog" "mouse") "mouse")
(check-expect (wordplay "hyphenate" "cat" "dog" "mouse") "cat-dog-mouse")

;; NOTE: The data definition below is only necessary if you chose to write a signature
;; for this function.

;; A WordPlayOp is one of:
;; - "first"
;; - "last"
;; - "hyphenate"

;; wordplay : WordPlayOp String String String -> String
;; Combine the strings with the given operation
(define (wordplay operation s1 s2 s3)
  (cond [(string=? operation "first") s1]
        [(string=? operation "last") s3]
        [(string=? operation "hyphenate")
         (string-append s1 "-" s2 "-" s3)]))