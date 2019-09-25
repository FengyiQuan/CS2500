;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname SMASH) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; find-product : LoN -> Number
(check-expect (find-product '()) 1)
(check-expect (find-product (cons 2 (cons 4 (cons 8 '())))) 64)

(define (find-product lon)
  (cond
    [(empty? lon) 1]
    [(cons? lon) (* (first lon)
                    (find-product (rest lon)))]))

;; SMASH : (X Y) Listof-X Y [X Y -> Y]-> Y
;; pairwise combines element  of a list with a base case
(check-expect (SMASH '() 1 *) 1)
(check-expect (SMASH '() 0 +) 0)

(define (SMASH l basecase funk)
  (cond
    [(empty? l) basecase]
    [(cons? l) (funk
                (first l)
                (SMASH (rest l) basecase funk))]))

(define (find-product.2 lon)
  (SMASH lon 1 *))