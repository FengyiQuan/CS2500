;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Midterm 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Problem 1
(define-struct roof (top))
(define-struct free (way speech))
; A Cowboy is one of:
; -- (make-roof String)
; -- (make-free Boolean Number) ; intepretation not needed
; intepretation not needed
;; Examples:
(define COWBOY1 (make-roof "abc"))
(define COWBOY2 (make-free #true 5))
(define COWBOY3 (make-free #false 10))

;; Problem 2
(define-struct brick (wall))
(define-struct fast (lane car))
; A Silly is one of:
; -- Integer
; -- (make-brick Number)
; -- (make-fast String Number) ; intepretation not needed
; intepretation not needed
;; Examples:
(define ex1 (make-brick "wall")) ; does not belong to Silly becasue "wall" is not Number
(define ex2 3.14) ; does not belong to Silly because 3.14 is not integer
(define ex3 (make-fast "hello" 3.14)) ; belong to Silly

;; Problem 5
(define-struct listing (name price more))
; A DB is one of:
; -- #false
; -- (make-listing String Number DB)
; interpretation a sequence of realty listings
; Examples:
(define DB0 #false)
(define DB1 (make-listing "abc" 5 DB0))
(define DB2 (make-listing "df" 10 DB1))

;; look : DB String -> Number
;; The function consumes a DB and a String. Its result is the first number associated
;; with the given String in the given DB. If it can’t find the String, it produces -1.
(check-expect (look DB0 "dfg") -1)
(check-expect (look DB1 "abc") 5)
(check-expect (look DB2 "abc") 5)
(define (look db str)
  (cond
    [(boolean? db) -1]
    [(listing? db) (if (string=? str (listing-name db))
                       (listing-price db)
                       (look (listing-more db) str))]))