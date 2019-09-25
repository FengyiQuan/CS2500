;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 18|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 1
; digit->string : [0, 9] -> String
; String representation of this digit
(check-expect (build-list 10 digit->string)
              (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define (digit->string d)
  (string (integer->char (+ 48 d))))

;; nat->string : Non-negative-interger -> String
;; given a non-negative integer returns its string form
;; Termination: Each time we recur, we discard the unit digit and
;;              reached a interger less than 10 which will end the program
(check-expect (nat->string 15) "15")
(check-expect (nat->string 0) "0")
(check-expect (nat->string 100) "100")
(check-expect (nat->string 123456) "123456")
(define (nat->string n)
  (local [;; append-string : Number String -> String
          ;; append a string and a string form of a given number
          ;; Given 9 "1", returns "91"
          (define (append-string digit s)
            (string-append (digit->string digit) s))]
    (foldr append-string "" (reverse (number->digits n)))))

;; number->digits : Non-negative-interger -> [List-of Number]
;; create a list of the individual digits of a number
;; Termination: Each time we recur, we discard the unit digit and
;;              reached a interger less than 10 which will end the program
(check-expect (number->digits 10) (list 0 1))
(check-expect (number->digits 0) (list 0))
(define (number->digits n)
  (cond
    [(< n 10) (list n)]
    [else (cons (modulo n 10) (number->digits (/ (- n (modulo n 10)) 10)))]))

;; Exercise 2
;; string-split : String String -> [List-of String]
;; given a string to split and a non-empty delimiter splits the string by that delimiter
;; Termination: Each time we recur, we discard the first part of string before a given delimiter
;;              reached an string which does not contain a given delimiter will end the program
(check-expect (string-split "" "s") (list ""))
(check-expect (string-split "hello%%bob%%%i%%%%am%%jack" "%%")
              (list "hello" "bob" "%i" "" "am" "jack"))
(check-expect (string-split "%%hello%%bob%%%i%%%%am%%jack%%" "%%")
              (list "" "hello" "bob" "%i" "" "am" "jack" ""))
(define (string-split s delimiter)
  (cond [(not (string-contains? delimiter s)) (list s)]
        [else (cons (get-first-part s delimiter) (string-split (get-rest s delimiter) delimiter))]))

;; get-first-part : String String -> String
;; get the first part of a string before a given delimiter
;; Termination: Each time we recur, we discard the first character and 
;;              reached an empty string which will end the program
(check-expect (get-first-part "asdfg" "d") "as")
(check-expect (get-first-part "" "d") "")
(define (get-first-part s delimiter)
  (cond
    [(string=? "" s) ""]
    [else (if (string=? (substring s 0 (string-length delimiter)) delimiter)
              ""
              (string-append (substring s 0 1) (get-first-part (substring s 1) delimiter)))]))

;; get-rest : String String -> String
;; gets the rest part of a string after a given delimiter
(check-expect (get-rest "" "s") "")
(check-expect (get-rest "asdfg" "s") "dfg")
(check-expect (get-rest "asdfghjkl" "sd") "fghjkl")
(define (get-rest s delimiter)
  (cond
    [(string=? "" s) ""]
    [else (substring s (+ (string-length (get-first-part s delimiter))
                          (string-length delimiter)))]))