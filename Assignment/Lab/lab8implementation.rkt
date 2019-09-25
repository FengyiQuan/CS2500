;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab8-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require "../Resources/mimic.rkt")
; A [Pair X Y] is a (list X Y)

; A [Mapping X Y] is a [List-of [Pair X Y]]
; and associates data of type X with data of type Y


; A [Counter X] is a [Mapping X PosInt]
; and represents a multiset

(define MARBLE-BAG (list (make-pair "green" 2) (make-pair "red" 5)))
; MARBLE-BAG represents a bag with 2 "green" marbles and 5 "red" ones

;; add-to-counter : [Counter X] X -> [Counter X]
;; Add 1 to x in c
(check-expect (add-to-counter MARBLE-BAG "green") (list (make-pair "green" 3)
                                                        (make-pair "red" 5)))
(check-expect (add-to-counter MARBLE-BAG "brown") (list (make-pair "green" 2)
                                                        (make-pair "red" 5)
                                                        (make-pair "brown" 1)))
(define (add-to-counter c x)
  (update-mapping c x add1 1))

; total-size : [Counter X] -> Number
; The total size of c
(check-expect (total-size MARBLE-BAG) 7)
(define (total-size c)
  (local [; plus : [Pair X PosInt] Number -> Number
          ; Sum the two numbers
          (define (plus p n)
            (+ (pair-y p) n))]
    (foldr plus 0 c)))

; expected-counts : [Counter X] Nat -> [List-of Number]
; Expected counts of elements when grabbing from the counter n times
(check-expect (expected-counts '() 100) '())
(check-expect (expected-counts MARBLE-BAG 1000)
              (list (* 2/7 1000) (* 5/7 1000)))
(define (expected-counts c n)
  (local [(define total (total-size c))
          ;; expected : [Pair X PosInt] -> Number
          ;; Expected count of this element when grabbing from c n times
          (define (expected p)
            (* (/ n total) (pair-y p)))]
    (map expected c)))

;; initiate-counter : X -> [Counter X]
;; A counter with one element, x
(check-expect (initiate-counter "dog") (list (make-pair "dog" 1)))
(define (initiate-counter x)
  (list (make-pair x 1)))

; count : [List-of X] X -> N
; How many times does x appear in the list?
(check-expect (count '() "b") 0)
(check-expect (count (list "a" "b" "a") "a") 2)
(define (count lox x)
  (local [;; equal-to-x? : X -> Boolean
          ;; Is it equal to x?
          (define (equal-to-x? x1)
            (equal? x x1))]
    (length (filter equal-to-x? lox))))

; count-grabs : [Counter X] [List-of X] -> [List-of N]
; See how many times the elements from this counter are in this list
(check-expect (count-grabs '() '()) '())
(check-expect (count-grabs MARBLE-BAG (list "red" "green" "red" "red")) (list 1 3))
(define (count-grabs c lox)
  (local [;; count-grab-of-x : X -> Number
          ;; How many times this element appeared in the list
          (define (count-grab-of-x c)
            (count lox (pair-x c)))]
    (map count-grab-of-x c)))

; grab-random : [Counter X] -> X
; Randomly grab from this counter
(define (grab-random c)
  (local (; grab : N [Counter X] -> X
          ; Grab the first element in c if its count is larger than n,
          ; otherwise reduce n by the count and recur
          (define (grab n c)
            (cond [(< n (pair-y (first c))) (pair-x (first c))]
                  [else (grab (- n (pair-y (first c))) (rest c))])))
    (grab (random (total-size c)) c)))

;; grab-n : [Counter X] Nat -> [List-of X]
;; Grab from the counter n times
(define (grab-n c n)
  (local [;; grab-once : ? -> X
          ;; Grab from c one
          (define (grab-once _)
            (grab-random c))]
    (build-list n grab-once)))

(check-within (count-grabs MARBLE-BAG (grab-n MARBLE-BAG 10000))
              (expected-counts MARBLE-BAG 10000)
              100)
; A WritingStyle is a [Mapping String [Counter String]]
; and represents how often some words follow another,
; along with what words start and end a sentence.
; The empty string is associated with words that start a sentence,
; and how many times a word ends a sentence can be
; determined by the count of "." in its associated Counter.

(define STYLE-EXAMPLE
  '(("great" (("." 1)))
    ("am" (("great" 1) ("i" 1)))
    ("i" (("am" 1) ("." 1)))
    ("" (("i" 1) ("how" 2)))
    ("how" (("am" 1) ("are" 1)))
    ("you" (("." 1)))
    ("are" (("you" 1)))))

; A Sentence is a [List-of String]

;; add-to-ws : WritingStyle String String -> WritingStyle
;; s2 follows s1 one more time
(check-expect (add-to-ws STYLE-EXAMPLE "great" ".") 
              '(("great" (("." 2)))
                ("am" (("great" 1) ("i" 1)))
                ("i" (("am" 1) ("." 1)))
                ("" (("i" 1) ("how" 2)))
                ("how" (("am" 1) ("are" 1)))
                ("you" (("." 1)))
                ("are" (("you" 1)))))
(check-expect (add-to-ws STYLE-EXAMPLE "great" "grim")
              '(("great" (("." 1) ("grim" 1)))
                ("am" (("great" 1) ("i" 1)))
                ("i" (("am" 1) ("." 1)))
                ("" (("i" 1) ("how" 2)))
                ("how" (("am" 1) ("are" 1)))
                ("you" (("." 1)))
                ("are" (("you" 1)))))
(check-expect (add-to-ws STYLE-EXAMPLE "gobble" "grim")
              '(("great" (("." 1)))
                ("am" (("great" 1) ("i" 1)))
                ("i" (("am" 1) ("." 1)))
                ("" (("i" 1) ("how" 2)))
                ("how" (("am" 1) ("are" 1)))
                ("you" (("." 1)))
                ("are" (("you" 1)))
                ("gobble" (("grim" 1)))))
(define (add-to-ws ws s1 s2)
  (local [;; add-s2-to-counter : [Counter String] -> [Counter String]
          ;; Add s2 to the counter
          (define (add-s2-to-counter c)
            (add-to-counter c s2))]
    (update-mapping ws s1 add-s2-to-counter (initiate-counter s2))))

;; update-ws : Sentence WritingStyle -> WritingStyle
;; Update the writing style
(check-expect (update-ws '("cat") STYLE-EXAMPLE) STYLE-EXAMPLE)
(check-expect (update-ws '("great" "am" "i") STYLE-EXAMPLE)
              '(("great" (("." 1) ("am" 1)))
                ("am" (("great" 1) ("i" 2)))
                ("i" (("am" 1) ("." 1)))
                ("" (("i" 1) ("how" 2)))
                ("how" (("am" 1) ("are" 1)))
                ("you" (("." 1)))
                ("are" (("you" 1)))))
(define (update-ws s ws)
  (cond [(< (length s) 2) ws]
        [else (add-to-ws (update-ws (rest s) ws) (first s) (second s))]))

;; style : [List-of Sentence] -> WritingStyle
;; Create a writing style
(check-expect (style '(("how" "are" "you") ("how" "am" "i") ("i" "am" "great"))) STYLE-EXAMPLE)
(define (style los)
  (local [;; add-s : Sentence WritingStyle -> WritingStyle
          ;; Add s to the writing style
          (define (add-s s ws)
            (update-ws (cons "" (append s (list "."))) ws))]
    (foldr add-s '() los)))

;; imitate : WritingStyle -> Sentence
;; Imitate ws
(define (imitate ws)
  (local [;; next-word : String -> Sentence
          ;; Keeps track of the current word and outputs a sentence
          (define (next-word current-word)
            (cond [(string=? current-word ".") '()]
                  [else (cons current-word (next-word (grab-random (get ws current-word))))]))]
    (rest (next-word ""))))

;; imitate-style : [List-of @tech{Sentence}] -> @tech{Sentence}
;; Given many of someone's sentences, output one like it
(define imitate-style (compose imitate style))

(imitate-style THE-RAVEN)