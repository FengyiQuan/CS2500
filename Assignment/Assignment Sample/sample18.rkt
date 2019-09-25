;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample18) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 1

;; nat->string : Nat -> String
;; String representation of the nat
;; TERMINATION: On each recursive call we are dividing the number by 10 so it gets smaller and
;; smaller until it is eventually less than 10
(check-expect (nat->string 0) "0")
(check-expect (nat->string 10234) "10234")
(define (nat->string n)
  (cond [(< n 10) (digit->string n)]
        [else (string-append (nat->string (quotient n 10))
                             (digit->string (modulo n 10)))]))

;; digit->string : [0, 9] -> String
;; String representation of this digit
(check-expect (build-list 10 digit->string)
              (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9"))
(define (digit->string d)
  (string (integer->char (+ 48 d))))

;; Exercise 2

;; string-split : String String -> [List-of String]
;; Split the string by the delimiter
;; TERMINATIONS: On each recursive call we are providing a smaller string (the string after the
;; given delimiter) so we will eventually reach a case where the string is too small to contain
;; the delimiter.
(check-expect (string-split "hello%%bob%%%i%%%%am%%jack" "%%")
              (list "hello" "bob" "%i" "" "am" "jack"))
(define (string-split str delimiter)
  (cond [(not (string-contains? delimiter str)) (list str)]
        [else (cons (before-delimiter str delimiter)
                    (string-split (after-delimiter str delimiter) delimiter))]))

;; before-delimiter : String String -> String
;; str before delimiter appears, assume delimiter appears
(check-expect (before-delimiter "%%bee" "%%") "")
(check-expect (before-delimiter "apple%%bee" "%%") "apple")
(check-expect (before-delimiter "apple%%" "%%") "apple")
(define (before-delimiter str delimiter)
  (cond [(string=? (substring str 0 (string-length delimiter)) delimiter) ""]
        [else (string-append (substring str 0 1)
                             (before-delimiter (substring str 1) delimiter))]))

;; after-delimiter : String String -> String
;; str after delimiter appears, assume delimiter appears
(check-expect (after-delimiter "%%bee" "%%") "bee")
(check-expect (after-delimiter "apple%%bee" "%%") "bee")
(check-expect (after-delimiter "apple%%" "%%") "")
(define (after-delimiter str delimiter)
  (cond [(string=? (substring str 0 (string-length delimiter)) delimiter)
         (substring str (string-length delimiter))]
        [else (after-delimiter (substring str 1) delimiter)]))