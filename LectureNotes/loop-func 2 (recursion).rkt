;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |loop-func 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct nn [prev])
; A NaturalNumber (NN) is one of:
; - 0
; - (make-nn NN)
; Interpretation: A natural number (counting number)

(define NN-0 0)
(define NN-1 (make-nn NN-0))
(define NN-2 (make-nn (make-nn NN-0)))
#;
(define (nn-temp nn)
  (cond
    [(number? nn) ...]
    [(nn? nn) ...
     (nn-temp (nn-prev nn))]))

; nn->int : NN -> NatNum
; Design the function nn->int that returns the Integer that the NN represents

(check-expect (nn->int NN-0) 0)
(check-expect (nn->int NN-1) 1)
(check-expect (nn->int NN-2) 2)

(define (nn->int nn)
  (cond
    [(number? nn) 0]
    [(nn? nn)
     (add1 (nn->int (nn-prev nn)))]))

; nn-add1 : NN -> NN
(check-expect (nn-add1 NN-0) NN-1)
(check-expect (nn-add1 NN-1) NN-2)

(define (nn-add1 nn)
  (make-nn nn))

; nn-even? : NN -> Boolean
; return #true if the input is even

(check-expect (nn-even? NN-0) #true)
(check-expect (nn-even? NN-1) #false)
(check-expect (nn-even? NN-2) #true)

;(define (nn-even? nn)
;  (even? (nn->int nn)))
(define (nn-even? nn)
  (cond
    [(number? nn) #true]
    [(nn? nn) (not
     (nn-even? (nn-prev nn)))]))
