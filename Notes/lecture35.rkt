;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lecture35) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;ANNOUNCEMENTS
;; 1. Homework 19 (the last ever homework in this course) is due tonight!
;; 2. Lecture on Wednesday will be a bit different. First we will do a Q+A for about 20
;;  minutes and then there will be an enrichment lecture (a lecture on a topic that you
;;  don't NEED for this course but something we think would be interesting using the skills
;;  you've learned thus far).

;; Remember how last time we were learning about lambda? Let's do some practice with that.

;; Design the function close-to-origin that accepts a Number and a [List-of Posn] and returns only
;; the Posns that are closer than the given distance from the origin

(define POSN1 (make-posn 0 0))
(define POSN2 (make-posn 3 4))
(define POSN3 (make-posn 100 50))
(define POSN4 (make-posn 2 1))

;; close-to-origin : Number [List-of Posn] -> [List-of Posn]
;; Returns only those Posns taht are closer than the given distance to the origin
(check-expect (close-to-origin 5 '()) '())
(check-expect (close-to-origin 5 (list POSN1 POSN2 POSN3 POSN4)) (list POSN1 POSN4))
(define (close-to-origin n lop)
  (filter (λ (p) (< (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))) n)) lop))

;; BECCA : Personally I would have written a "distance-to-origin" function separately
;; and just called it and checked if it was less than n.

;; Design the function sort-by-length that accepts a [List-of String] and rturns a version
;; sorted by the length of the string (shortest first).

;; sort-by-length : [List-of String] -> [List-of String]
;; Sorts the given list of strings by shortest length first
(check-expect (sort-by-length '()) '())
(check-expect (sort-by-length (list "abc" "d" "ef")) (list "d" "ef" "abc"))
(check-expect (sort-by-length (list "x" "a" "b")) (list "x" "a" "b"))
(define (sort-by-length los)
  (sort los (λ (s1 s2) (< (string-length s1) (string-length s2)))))

;; Design the function first-squares that accepts a Nat N and returns the first N squares

;; first-squares : Nat -> [List-of Nat]
;; Returns the first N perfect squares
(check-expect (first-squares 0) '())
(check-expect (first-squares 3) (list 1 4 9))
(define (first-squares n)
  (build-list n (λ (x) (sqr (add1 x)))))

;; Without using lambda, design a function only-starts-with-hello, which accepts a
;; [List-of String] and keeps only those beginning with "hello".

;; only-starts-with-hello : [List-of String] -> [List-of String]
;; Returns a list of the strings that start with "hello" in the given list
(check-expect (only-starts-with-hello '()) '())
(check-expect (only-starts-with-hello (list "apple" "hello you" "goodbye hello" "hello Bob"))
              (list "hello you" "hello Bob"))
(define (only-starts-with-hello los)
  (filter starts-with-hello? los))

;; String -> Boolean
;; Does this string start with "hello"?
(check-expect (starts-with-hello? "") #false)
(check-expect (starts-with-hello? "hello you") #true)
(check-expect (starts-with-hello? "goodbye hello") #false)
(define (starts-with-hello? s)
  (and (>= (string-length s) (string-length "hello"))
       (string=? (substring s 0 (string-length "hello")) "hello")))

;; But what if we also want a function that finds the strings starting with "goodbye" or a
;; function that finds the strings starting with "fundies" or something else? We need to
;; abstract. But filter requires a function of one argument...

;; What we will do is build a function that returns a function. It's a sort of "function factory"
;; which will produce functions for you. Those are the functions we need in filter.

;; starts-with? : String -> [String -> Boolean]
;; Returns a predicate that returns #true if the given string starts with prefix
(check-expect ((starts-with? "hello") "hello you") #true)
(check-expect ((starts-with? "abc") "") #false)
(define (starts-with? prefix)
  (λ (str) (and (>= (string-length str) (string-length prefix))
                (string=? (substring str 0 (string-length prefix)) prefix))))

;; Now it is super quick to write all our filtering functions.

;; only-starts-with-goodbye : [List-of String] -> [List-of String]
;; Returns a list of the strings that start with "goodbye" in the given list
(check-expect (only-starts-with-goodbye '()) '())
(check-expect (only-starts-with-goodbye (list "goodbye" "hello you" "goodbye you" "hi goodbye"))
              (list "goodbye" "goodbye you"))
(define (only-starts-with-goodbye los)
  (filter (starts-with? "goodbye") los))

;; Note that we are calling a function to get a function here. Normally you don't want to call your
;; function inside filter, since filter takes the function itself and not its result, but in this
;; case our result IS a function.