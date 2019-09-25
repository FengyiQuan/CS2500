;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |overall funk|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; - do-to-all: Do the same thing to every element in a list
;; do-to-all : (X Y) [List-of X] [X -> Y] -> [List-of Y]
;; Apply the given function to each number in the list
(define (do-to-all lon operation)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (operation (first lon))
                           (do-to-all (rest lon) operation))]))

;; - keep-if: Keep only the elements of a list that pass a certain test
;; keep-if / check-if : (X) [List-of X] [X -> Boolean] -> [List-of X]
;; Returns a list of elements that pass the test
(define (keep-if lox test)
  (cond [(empty? lox) lox]
        [(cons? lox)
         (if (test (first lox))
             (cons (first lox) (keep-if (rest lox) test))
             (keep-if (rest lox) test))]))

;; - collapse: Combine all the elements of a list in some way
;; collapse / SMASH : (X Y) [List-of X] Y [X Y -> Y] -> Y
;; Repeatedly call the given function on each element of the list
(define (collapse lox base-case operation)
  (cond [(empty? lox) base-case]
        [(cons? lox) (operation (first lox) (collapse (rest lox) base-case operation))]))

;; Design a function sum-of-string-lengths that uses
;; SUMASH!! to add up all the lengths of strings in a list

;; sum-of-string-lengths : [List-of String] -> Numbers

(check-expect (sum-of-string-lengths '()) 0)
(check-expect (sum-of-string-lengths (list "a" "bb" "ccc" )) 6)

(define (sum-of-string-lengths los)
  (collapse los 0 add-string-length))

;; add-string-length : String Number -> Number
;; adds the string's length to the current sum

(check-expect (add-string-length "a" 0) 1)
(check-expect (add-string-length "a" 10) 11)
(check-expect (add-string-length  "abc" 10) 13)
(define (add-string-length s n)
  (+ (string-length s) n))