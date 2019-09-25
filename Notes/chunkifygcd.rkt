;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname chunkify-gcd) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design a function chunkify that accepts a [List-of X] and a positive
; integer (PosInt), and returns a [List-of [List-of X]] where each of
; the inner lists is this many elements long, except potentially the last.

; chunkify : [List-of X] Nat -> [List-of [List-of X]]
; Groups the input list into sub-lists of given length
(check-expect (chunkify (list 1 2 3)       1) (list (list 1) (list 2) (list 3)))
(check-expect (chunkify (list 1 2 3)       2) (list (list 1 2) (list 3)))
(check-expect (chunkify (list 1 2 3 4 5 6) 3) (list (list 1 2 3) (list 4 5 6)))
(define (chunkify l n)
  (if (<= (length l) n)
      (list l)
      (cons (first-n l n) (chunkify (drop-n l n) n))))

; Termination argument: ??

; first-n : [List-of X] Nat -> [List-of X]
; return the first n elements of l, if they exist
(check-expect (first-n (list 1 "a" (list "b")) 2) (list 1 "a"))
(check-error  (first-n (list 1 "a" (list "b")) 4))
(define (first-n l n)
  (cond [(= n 0)    '()]
        [(empty? l) (error "list too short")]
        [else       (cons (first l) (first-n (rest l) (- n 1)))]))

; drop-n : [List-of X] Nat -> [List-of X]
; return all BUT the first n elements of l, if they exist
(check-expect (drop-n (list 1 "a" (list "b")) 2) (list (list "b")))
(check-error  (drop-n (list 1 "a" (list "b")) 4))
(define (drop-n l n)
  (cond [(= n 0)    l]
        [(empty? l) (error "list too short")]
        [else       (drop-n (rest l) (- n 1))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Design the function GCD that accepts two positive integers (PosInt)
; and computes their greatest common divisor.

; GCD : PosInt PosInt -> PosInt
; Computes the GCD
(check-expect (GCD  6  4)  2)
(check-expect (GCD 27 81) 27)
(check-expect (GCD 36 81)  9)
(define (GCD x y)
  (local [(define (CD i)
            (= (remainder x i) (remainder y i) 0))
          (define (GCD-<= i)
            (if (CD i)
                i
                (GCD-<= (sub1 i))))]
    (GCD-<= (min x y))))
; Termination argument: ??

; Try something like
; (GCD 10822865089 10968163441)
; (use two large random integers)

(check-expect (GCD.v2  6  4)  2)
(check-expect (GCD.v2 27 81) 27)
(check-expect (GCD.v2 36 81)  9)
(define (GCD.v2 x y)
  (cond [(> y x)               (GCD.v2 y x)]
        [(= (remainder x y) 0) y]
        [else                  (GCD.v2 y (- x y))])) ; can also use this as the recursive call: (GCD.v2 y (remainder x y))

; Now try again:
; (GCD.v2 10822865089 10968163441)
