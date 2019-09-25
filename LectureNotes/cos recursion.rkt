;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |cos recursion|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A List-of-Strings (LoS) is:
; - '()
; - (cons String LoS)
; Interpretation: List of words

(define LoS-0 '())
(define LoS-1 (cons "hello" LoS-0))
(define LoS-2 (cons "hodi" LoS-1))
(define LoS-3 (cons "ood" LoS-2))
#;
(define (los-temp los)
  (cond
    [(empty? los) ...]
    [(cons? los) ...
     (first los) ...
     (los-temp (rest los)) ...]))

; Design the function los-count that
; counts up how many strings are in a LoS
;; los-count : LoS -> NonNegativeNumber
(check-expect (los-count LoS-0) 0)
(check-expect (los-count LoS-1) 1)
(check-expect (los-count LoS-2) 2)
(define (los-count los)
  (cond
    [(empty? los) 0]
    [(cons? los) (add1 (los-count (rest los)))]))

; Design the function los-char-coumt that
; counts up how many characters are in all
; the strings in an LoS
;; los-char-count : LoS -> NonNegativeNumber
(check-expect (los-char-count LoS-0) 0)
(check-expect (los-char-count LoS-1) 5)
(check-expect (los-char-count LoS-2) 9)
(define (los-char-count los)
  (cond
    [(empty? los) 0]
    [(cons? los) (+ (string-length (first los))
                    (los-char-count (rest los)))]))

; Design the function ood? that returns
; #true if the list contains a string that
; contains the string "odd"
; ood? : LoS -> Boolean
(check-expect (ood? LoS-0) #false)
(check-expect (ood? LoS-1) #false)
(check-expect (ood? LoS-2) #false)
(check-expect (ood? LoS-3) #true)
(define (ood? los)
  (cond
    [(empty? los) #false]
    [(cons? los) (if (string-contains? "ood" (first los)) #true
                     (ood? (rest los)))]))

(check-expect (ood1? LoS-0) #false)
(check-expect (ood1? LoS-1) #false)
(check-expect (ood1? LoS-2) #false)
(check-expect (ood1? LoS-3) #true)
(define (ood1? los)
  (cond
    [(empty? los) #false]
    [(cons? los) (or (string-contains? "ood" (first los))
                     (ood1? (rest los)))]))

; A List-of-Posms (LoP) is:
; - '()
; - (cons Posn LoP)
; Examples:
(define LoP-0 '())
(define LoP-1 (cons (make-posn 1 2) LoP-0))
(define LoP-2 (cons (make-posn 3 4) LoP-1))
#;
(define (lop-temp lop)
  (cond
    [(empty? lop) ...]
    [(cons? lop) ...
     (posn-temp (first lop)) ...
     (lop-temp (rest lop)) ...]))

; Design a function max-distance-from-origin
; that returns that maximum distance from the origin
; given a LoP
; max-distance-from-origin : LoP -> NonNegNumber
(check-expect (max-distance-from-origin LoP-0) 0)
(check-within (max-distance-from-origin LoP-1) (sqrt 5) 0.00000001)
(check-within (max-distance-from-origin LoP-2) 5 0.00000001)
(define (max-distance-from-origin lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop)
     (max (distance-from-origin (first lop))
          (max-distance-from-origin (rest lop)))]))

; distance-from-origin : Posn -> NonNegNumber
; computes the distance from the origin of a position
(check-within (distance-from-origin (make-posn 1 2)) (sqrt 5) 0.00000001)
(check-expect (distance-from-origin (make-posn 3 4)) 5)
(define (distance-from-origin lop)
  (sqrt (+ (sqr (posn-x lop))
           (sqr (posn-y lop)))))


