;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lambda prac|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; first-squares : Nat -> [List-of Number]
; returns the first N squares
(define (first-squares n)
  (map (λ (x) (sqr x)) (build-list n add1)))

;; only-starts-with-hello : [List-of String] -> [List-of String]
;; accepts a [List-of String] and keeps only those beginning with "Hello"
(check-expect (only-starts-with-hello (list "" "Hello" "Helloooooooo" "oooooo"))
              (list "Hello" "Helloooooooo"))

(define (only-starts-with-hello los)
  (filter starts-with-hello? los))

;; starts-with-hello? : String -> Boolean
(define (starts-with-hello? s)
  (and (>= (string-length s) 5)
       (string=? (substring s 0 5) "Hello")))



(define (only-starts-with-word word los)
  (filter (make-starts-wiht-words? word) los))

;; make-starts-wiht-words? : String -> [String -> Boolean]
(define (make-starts-wiht-words? word)
  (λ (s)
    (and (>= (string-length s) (string-length word))
         (string=? (substring s 0 (string-length word)) word))))





;; only-starts-with-hi : [List-of String] -> [List-of String]
(define (only-starts-with-hi los)
  (only-starts-with-word "Hi" los))