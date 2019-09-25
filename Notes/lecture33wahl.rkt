;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture-33-wahl) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Design a function that reverses a list.
; (And DO NOT use (reverse ...) or (foldl ...).

; First write a version based on Structural Recursion. (Follow the Design Recipe.)

; rev : [List-of X] -> [List-of X]
; reverse the given list
(check-expect (rev (list 1 2 3)) (list 3 2 1))
(check-expect (rev (list "a" '() '() 10)) (list 10 '() '() "a"))
(check-expect (rev '()) '())
(define (rev lox)
  (cond [(empty? lox) '()]
        [(cons?  lox) (append (rev (rest lox)) (list (first lox)))]))

; (time (first (rev (build-list 100 identity))))
; (time (first (rev (build-list 10000 identity))))

; REV USING ACCUMULATOR

(define (rev.v2 lox)
  (local [(define (rev.v2-acc lox acc)
            (cond [(empty? lox) acc]
                  [(cons?  lox) (rev.v2-acc (rest lox) (cons (first lox) acc))]))]
    (rev.v2-acc lox '())))

; (time (first (rev.v2 (build-list 100 identity))))
; (time (first (rev.v2 (build-list 10000 identity))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Design a function that negates a predicate.

; negate : (X) [X -> Boolean] -> [X -> Boolean]
; negates the given predicate
(check-expect (map (negate even?) (list 1 2 3 4)) (list #true #false #true #false))
(define (negate f)
  (local [(define (negate-f x)
            (not (f x)))]
    negate-f))
(define my-odd? (negate even?))
(check-expect (my-odd? 10) #false)
(check-expect (my-odd? 11) #true)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Design function majority which takes a non-empty list of X and
; returns the most frequent element (if multiple, any one of them)

; majority : (X) [NEList-of X] -> X
; returns the most frequent element of the list
(check-expect (majority (list 1 2 2))                 2)
(check-expect (majority (list 1 "a" "a" 2 1 "b" 2 1)) 1)
(check-expect (or (= (majority (list 1 2)) 1)
                  (= (majority (list 1 2)) 2))
              #t)

(define-struct count [x n])

; A Count is a (make-count X Nat)
; An element and how often it occurs in some context
(define Count-0 (make-count "Hello" 1))

; get-max : [List-of Count] -> X
; finds one element with the highest count
(check-expect (get-max (list (make-count 1 2) (make-count 2 3))) 2)
(define (get-max loc)
  (local [(define (>-count x y) (if (> (count-n x) (count-n y)) x y))]
    (count-x (foldr >-count (make-count 0 0) loc))))

(define (majority lox)
  (local [; update : X [List-of Count] -> [List-of Count]
          ; updates the count of x
          (define (update x loc)
            (cond [(empty? loc) (list (make-count x 1))]
                  [(cons?  loc) (if (eq? (count-x (first loc)) x)
                                    (cons (make-count x (add1 (count-n (first loc)))) (rest loc))
                                    (cons (first loc) (update x (rest loc))))]))

          ; compute-frequencies : [List-of X] -> [List-of Count]
          ; returns the list of counts for each element of lox
          (define (compute-frequencies lox)
            (cond [(empty? lox) '()]
                  [(cons?  lox) (update (first lox) (compute-frequencies (rest lox)))]))]
    (get-max (compute-frequencies lox))))
