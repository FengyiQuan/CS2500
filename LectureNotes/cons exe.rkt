;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |cons exe|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-Numbers (LoN) is one of:
; - '()
; - (cons Number LoN)
; Interpretation: a list of numbers

(define LON-0 '())
(define LON-1 (cons 42 LON-0))
(define LON-2 (cons 3.14 LON-1))
#;
(define (lon-temp lon)
  (cond
    [(empty? lon) ...]
    [(cons? lon) ...
     (first lon) ...
     (lon-temp (rest lon)) ...]))

; lon-length : LoN -> NatNum
; counts the number of numbers in a list of numbers

(check-expect (lon-length LON-0) 0)
(check-expect (lon-length LON-1) 1)
(check-expect (lon-length LON-2) 2)

(define (lon-length lon)
    (cond
    [(empty? lon) 0]
    [(cons? lon) (add1 (lon-length (rest lon)))]))




