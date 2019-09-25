;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname local) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; (local [defines] expr)
(define z 3)
(local [(define x 10)
        (define y 1)]
  (+ x y z))


(define one 1)
(local [(define weird (add1 one))
        (define weirder (add1 weird))]
  (+ one weirder))

(define myfunnyname +)
(define main
  (local [(define (helper x) x)
          (define (foo z) (helper (+ 2 z)))]
    foo))
(main 10)

; Design a function addn that takes a Number and a [List-of Number]
; and returns a [list-of number] with the supplied number added
; to all the elements
(check-expect (addn 5 (list 1 2 3 4 5)) (list 6 7 8 9 10))
(define (addn n lon)
  (local [(define (adder element)
            (+ element n))]
    (map adder lon)))
  map add5 lon