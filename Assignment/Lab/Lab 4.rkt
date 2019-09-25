;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Lab  4
; A PRS (PetRockStorage) is one of:
; - "King Paimon"
; - (make-bag String Number PRS)
(define-struct bag [color size contents])
; and represents the pet rock
; or a bag containing it (and possibly other bags) with a specific color and size

;; Exercise 1
;; Examples:
(define PRS-0 "King Paimon")
(define PRS-1 (make-bag "red" 2 PRS-0))
(define PRS-2 (make-bag "green" 3 PRS-1))
(define PRS-3 (make-bag "aquamarine" 5 PRS-2))
#;
(define (prs-temp prs)
  (cond [(string? prs) ...]
        [(bag? prs) ... (bag-color prs) ...
                    ... (bag-size prs) ...
                    ... (prs-temp (bag-contents prs)) ...]))

;; Exercise 2
;; largest-bag : Bag -> Number
;; gives the size of the largest bag in a PRS
(check-expect (largest-bag PRS-0) 0)
(check-expect (largest-bag PRS-1) 2)
(check-expect (largest-bag PRS-2) 3)
(check-expect (largest-bag PRS-3) 5)
(define (largest-bag prs)
  (cond [(string? prs) 0]
        [(bag? prs) (max (bag-size prs)
                         (largest-bag (bag-contents prs)))]))

;; Exercise 3






