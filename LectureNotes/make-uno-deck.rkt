;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname make-uno-deck) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; mymax : [NEList-of Number] -> Number
;; Returns the biggest element in the list
(check-expect (mymax (list 3)) 3)
(check-expect (mymax (list 3 21 8 62 4 17)) 62)
(define (mymax neln)
  (cond [(empty? (rest neln)) (first neln)]
        [(cons? (rest neln))
         (local [(define MAX (mymax (rest neln)))]
           (if (>= (first neln) (mymax (rest neln)))
               (first neln)
               (mymax (rest neln))))]))

;; Design the function random-element that accpets a [List-of X]
; and returns a random element from it.
(define (random-element lox)
  (list-ref lox (random (length lox))))

;; Design the function mysort that accepts a [List-of Number]
;; and returns a sorted version of that [List-of Number]
;; Use a list abstraction, but not sort

; mysort : [List-of Number] -> [SortedList-of Number]

(check-expect (mysort (list 8 6 7 5 3 0 9)) (list 0 3 5 6 7 8 9))
(define (mysort.v1 lon)
  (local [;; add-to-sorted : Number [SortedList-of Number] -> [SortedList-of Number]
          ;; adds a number to a sorted list
          (define (add-to-sorted n slon)
            (cond
              [(empty? slon) (list n)]
              [(cons? slon) (if (< n (first slon))
                                (cons n slon)
                                (cons (first slon) (add-to-sorted n (rest slon))))]))]
    (foldr add-to-sorted '() lon)))

(define (mysort lon)
  (sort lon <))

;; Design the function rev that accepts a [List-of X] and reverses it.
; You must use a pre-defined list abstraction
(check-expect (rev (list 8 6 7 5 3 0 9)) (list 9 0 3 5 7 6 8))
(define (rev l)
  (foldl cons '() l))

;---------------------------------------------------------------
(define-struct card [number clolr])
; An UnoCard is a (make-card Number String)

; Design the function make-uno-deck that accepts a Nat
; representing the highest numeric card value and a [List-of String]
; representing the color, and generates a deck of Uno cards
; (as a [List-of Card]). There should be one card with each color and number
#;(check-expet (make-uno-deck 2 (list "red" "green"))
               (list (make-card 0 "red") (make-card 1 "red") (make-card 2 "red")
                     (make-card 0 "green") (make-card 1 "green") (make-card 2 "green")))

; make-uno-deck : Nat [List-of String] -> [List-of UnoCard]
(define (make-uno-deck n colors)
  (local [; append-to-color-card-list : String [List-of UnoCard] -> [List-of UnoCard]
          (define (append-to-color-card-list color deck)
            (append make-color-cards deck))]
    (foldr append-to-color-card-list '() colors)))
; make-color-cards : Nat String -> [List-of UnoCard]
(check-expect (make-color-cards 2 "red")
              (list (make-card 0 "red") (make-card 1 "red") (make-card 2 "red")))

(define (make-color-cards n color)
  (local [; make-card : Nat -> UnoCard
          (define (make-color-card num)
            (make-card num color))]
    (build-list (add1 n) make-color-card)))