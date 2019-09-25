;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname check-if-wrong) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; do-to-all : (X Y) Listof-X [X -> Y] -> Listof-Y
(define (do-to-all l funk)
  (cond
    [(empty? l) '()]
    [(cons? l) (cons (funk (first l))
                     (do-to-all (rest l) funk))]))

; A ListofStrings (LoS) 
(define STR-1 "hi")
(define STR-2 "Dear Alice")
(define STR-3 "asdfljasdfjaosdfjasdkfjasdlfajl")

(define LOS-0 '())
(define LOS-1 (cons STR-1 LOS-0))
(define LOS-2 (cons STR-2 LOS-1))
(define LOS-3 (cons STR-3 LOS-2))

polite? : 
;; RIGHT SIDE : design polite-msgs

;; polite-msgs : 
; that accepts a ListofStrings
; and returns just those that start with "Dear"
(check-expect (polite-msgs LOS-0) LOS-0)
(check-expect (polite-msgs LOS-1) LOS-0)
(check-expect (polite-msgs LOS-2) (cons STR-2 '()))
(check-expect (polite-msgs LOS-3) (cons STR-2 '()))

(define (polite-msgs los)
  (cond
    [(empty? los) los]
    [(cons? los) (if (polite? (first los)))]))
;-----------------------------
; check-if : (X) Listof-X [X -> Boolean] -> Listof-X
; keeps all the elements in the input if they pass the test
(define (check-if l p?)
  (cond
    [(empty? l) l]
    [(cons? l) (if (p? (first l))
                 
             (cons (first lox) (keep-if (rest lox) test))
             (keep-if (rest lox) test))]))]))