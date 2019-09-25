;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname Similarity) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Examples:
(define LOS-0 '())
(define LOS-1 (cons "hello" LOS-0))
(define LOS-2 (cons "george" LOS-1))

;; prefix-with-to : LOS -> LOS
;;accepts a ListofStrings and prefixes ecery string with "To: "
(check-expect (prefix-with-to LOS-0) LOS-0)
(check-expect (prefix-with-to LOS-1) (cons "To: hello" LOS-0))
(check-expect (prefix-with-to LOS-2) (cons "To: george" (cons "To: hello" '())))
(define (prefix-with-to los)
  (cond
    [(empty? los) los]
    [(cons? los) (cons (string-append "To: " (first los))
                       (prefix-with-to (rest los)))]))

;; prefix-with : LoS String -> String
; prefix every string in the input
(check-expect (prefix-with LOS-0 "hello") LOS-0)
(check-expect (prefix-with LOS-1 "bye") (cons "byehello" '()))
(check-expect (prefix-with LOS-2 "nate") (cons "nategeorge" (cons "natehello" '())))
(define (prefix-with los prefix)
  (cond
    [(empty? los) los]
    [(cons? los) (cons (string-append prefix (first los))
                       (prefix-with (rest los) prefix))]))

;; prefix-with-to.2 : LOS -> LOS
(define (prefix-with-to.2 los)
  (prefix-with los "To: "))
;-----------------------------------------------------------------------------------------
(define LON-0 '())
(define LON-1 (cons 25 '()))
(define LON-2 (cons 16 LON-1))

;; sqr-of-all : LoN -> LoN
(check-expect (sqr-of-all LON-0) LON-0)
(check-expect (sqr-of-all LON-1) (cons 625 '()))
(check-expect (sqr-of-all LON-2) (cons 256 (cons 625 '())))
(define (sqr-of-all lon)
  (cond
    [(empty? lon) lon]
    [(cons? lon) (cons (sqr (first lon))
                       (sqr-of-all (rest lon)))]))

; function-of-all : LoN Function-that-inputs-Number-and-outpus-Number -> LoN
; applies a maths function to all the elements in a list
(check-expect (function-of-all LON-0 sqr) LON-0)
(check-expect (function-of-all LON-1 add1) (cons 26 '()))
(check-expect (function-of-all LON-2 sub1) (cons 15 (cons 24 '())))
(define (function-of-all lon funk)
  (cond
    [(empty? lon) lon]
    [(cons? lon) (cons (funk (first lon))
                       (function-of-all (rest lon) funk))]))
(define (sqr-of-all.2 lon)
  (function-of-all lon sqr))
;-----------------------------------------------------------------------------------------
(define POSN-0 (make-posn 0 0))
(define POSN-1 (make-posn 3 4))
(define LOP-0 '())
(define LOP-1 (cons POSN-0 LOP-0))
(define LOP-2 (cons POSN-1 LOP-1))

(check-expect (euclidean-all LOP-0) '())
(check-expect (euclidean-all LOP-1) (cons 0 '()))
(check-expect (euclidean-all LOP-2) (cons 5 (cons 0 '())))

;; euclidean-all : LoP -> LoN
(define (euclidean-all lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (cons (euclidean-each (first lop))
                       (euclidean-all (rest lop)))]))
; euclidean-each : Position -> Number
; computes the manhattan distance from the position to the origin
(check-expect (euclidean-each POSN-0) 0)
(check-expect (euclidean-each POSN-1) 5)
(check-expect (euclidean-each (make-posn -3 4)) 5)
(define (euclidean-each posn)
  (sqrt (+ (sqr (posn-x posn))
           (sqr (posn-y posn)))))

;; distance-all : LoP Function-from-Position-to-Number -> LoN
;; computes distance to the origin given some distance function
(define (distance-all lop funk)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (cons (funk (first lop))
                       (distance-all (rest lop) funk))]))
;; euclidean-all.2
(define (euclidean-all.2 lop)
  (distance-all lop euclidean-each))



