;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |big template|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Design a func append-lists that accpets two [List-of Number],
;; and returns a list containing all of the elements of the first
;; followed by all of the elements of the second

; append-lists : [List-of Number] [List-of Number] -> [List-of Number]
(check-expect (append-lists (list 1 2 3) (list 3 4 5)) (list 1 2 3 3 4 5))

(define (append-lists l1 l2)
  (cond
    [(empty? l1) l2]
    [(cons? l1) (cons (first l1)
                      (append-lists (rest l1) l2))]))
;; -----------------------------------------------------
#;
(define (nat-los-temp n los)
  (cond
    [(and (zero? n) (empty? los)) ...]
    [(and (positive? n) (empty? los)) ... (nat-los-temp (sub1 n) los) ...]
    [(and (zero? n) (cons? los)) ... (first los) ... (nat-los-temp n (rest los)) ...]
    [(and (positive? n) (cons? los)) ... (first los) ... (nat-los-temp (sub1 n) (rest los)) ...]))


; StringOrFalse (SoF) is one of:
; - String
; - #f
(define SOF-1 "booooo!!!:)")
(define SOF-2 #f)

(define (sof-temp sof)
  (cond
    [(string? sof) ...]
    [(boolean? sof) ...]))
;; Design a function get-element that accepts a [List-of String] and a Nat
;; and returns the element at the location in the list specified by the Nat.
; If there is no such element, it should return #f

; get-element : [List-of String] Nat -> StringOrFalse
(check-expect (get-element (list "a" "b" "c") 0) "a")
(check-expect (get-element (list "a" "b" "c") 1) "b")
(check-expect (get-element (list "a" "b" "c") 2) "c")
(check-expect (get-element (list "a" "b" "c") 3) #f)

(define (get-element los nat)
  (cond
    [(empty? los) #f]
    [(and (zero? nat) (cons? los)) (first los)]
    [(and (positive? nat) (cons? los)) (get-element (rest los) (sub1 nat))]))

;; Design the function intersect that takes two [List-of String]
;; and returns those elements of the first that appear in the
;; second (in the order they appear in the first).

;; intersect : [List-of String] [List-of String] -> [List-of String]
(check-expect (intersect (list "a" "b" "b" "c" "a") (list "b" "o" "o")) (list "b" "b"))
(check-expect (intersect (list "a" "b" "b" "c" "a") (list "b" "a" "o")) (list "a" "b" "b" "a"))
(check-expect (intersect (list "b" "o" "o") '()) '())
(check-expect (intersect (list "a" "b" "b" "c" "a") '()) '())

(define (intersect l1 l2)
  (local [;; in-l2? : String -> Boolean
          (define (in-l2? s1)
            (local [;; is-s1? : String -> Boolean
                    (define (is-s1? s2)
                      (string=? s1 s2))]
              (ormap is-s1? l2)))]
    (filter in-l2? l1)))