;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture21) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Today we're going to do some practice with list abstractions and local.

;; First we answered some questions:
;; 1. Sometimes you will need nested locals. Obviously we don't want you to just stick
;;    every function you write in a local as this gets real gross real fast, but sometimes
;;    you need a nested local and that's okay.
;; 2. The difference between foldr and foldl is that one works left to right and
;;    the other works right to left.

;; Last time we talked about mymax and how our function became faster with local.
;; Here are the two versions of that function.

;; mymax : [NEList-of Number] -> Number
;; Returns the biggest element in the list
(check-expect (mymax (list 3)) 3)
(check-expect (mymax (list 3 21 8 62 4 17)) 62)
(define (mymax neln)
  (cond [(empty? (rest neln)) (first neln)]
        [(cons? (rest neln))
         (if (>= (first neln) (mymax (rest neln)))
             (first neln)
             (mymax (rest neln)))]))

;; mymax.v2 : [NEList-of Number] -> Number
;; Returns the biggest element in the list
(check-expect (mymax.v2 (list 3)) 3)
(check-expect (mymax.v2 (list 3 21 8 62 4 17)) 62)
(define (mymax.v2 neln)
  (cond [(empty? (rest neln)) (first neln)]
        [(cons? (rest neln))
         (local [(define MAXREST (mymax (rest neln)))]
           (if (>= (first neln) MAXREST)
               (first neln) MAXREST))]))

;; You can use the 'time' function to see the difference in time taken
;; to run (mymax (build-list 25 identity)) vs (mymax.v2 (build-list 25 identity))

(define REVERSED-LIST (reverse (build-list 25 identity)))

;; Try running (time (mymax REVERSED-LIST)). This is suddenly very fast! Why is that?
;; It's because if the first element is the biggest, we return the first element. But
;; if the first element is the smallest we keep having to call mymax over and over again.

;; Basically mymax.v2 avoids this problem by naming the result of the recursive call so
;; that we only have to calculate it once and we can use it in multiple places.

;; Okay let's talk about the homework. In the homework you're going to need to use list-ref which
;; is a function we haven't yet talked about. You can look it up in the docs for more information.

;; random-element : [NEList-of X] -> X
;; Returns a random element from the list
(check-expect (random-element (list "hello")) "hello")
(check-random (random-element (list 1 2 3 4)) (list-ref (list 1 2 3 4) (random 4)))
(define (random-element nelox)
  (list-ref nelox (random (length nelox))))

;; my-sort : [List-of Number] -> [List-of Number]
;; Returns a sorted version of the list (from least to greatest)
(check-expect (my-sort '()) '())
(check-expect (my-sort (list 99 2 4 0)) (list 0 2 4 99))
(define (my-sort lon)
  (local [;; Number [List-of Number] -> [List-of Number]
          ;; Insert a single number into the sorted list
          (define (insert-num n sorted)
            (cond [(empty? sorted) (list n)]
                  [(cons? sorted)
                   (if (> n (first sorted))
                       (cons (first sorted) (insert-num n (rest sorted)))
                       (cons n sorted))]))]
    (foldr insert-num '() lon)))

;; BECCA: I think insert-num should really be outside a local. It seems like it might be
;; generically useful and it's certainly more than a one line function. In order to test it
;; I personally would have pulled it out, but the rules for when to use local are "more like
;; guidelines anyway" so it's a personal design decision as to whether this is too complicated
;; or not. Professor Mislove left it in the local.

;; You may have noticed that there is a list abstraction called 'sort'. We can use sort to define
;; our function much more easily.

;; mysort.v2 : [List-of Number] -> [List-of Number]
;; Sort a list of numbers in increasing order
(check-expect (mysort.v2 '()) '())
(check-expect (mysort.v2 (list 99 2 4 0)) (list 0 2 4 99))
(define (mysort.v2 lon)
  (sort lon <))

;; rev : [List-of X] -> [List-of X]
;; Reverse the order of the elements in the list
(check-expect (rev '()) '())
(check-expect (rev (list 1 2 3 4)) (list 4 3 2 1))
(define (rev lox)
  (foldl cons '() lox))

;; Let's talk about the game UNO. Cards in UNO have a color and a number (mostly).

(define-struct card [number color])
;; An UnoCard is a (make-card Number String)

(define CARD-RED0 (make-card "red" 0))
(define CARD-RED1 (make-card "red" 1))
(define CARD-BLUE0 (make-card "blue" 0))
(define CARD-BLUE1 (make-card "blue" 1))

;; unocard-temp : UnoCard -> ???
(define (unocard-temp uc)
  (... (card-number uc) ... (card-color uc) ...))

(define DECK-REDBLUE (list CARD-RED0 CARD-RED1 CARD-BLUE0 CARD-BLUE1))

;; Design the function make-uno-deck that accepts a Nat representing the highest numeric value and a
;; [List-of String] representing the colors of the cards and generates a deck of UNO cards. There
;; should be one card with each color and number.

;; make-uno-deck : Nat [List-of String] -> [List-of UnoCard]
;; Generate a deck of UNO cards with all the given colors and numbers
(check-expect (make-uno-deck 1 (list "blue")) (list CARD-BLUE0))
(check-expect (make-uno-deck 2 (list "red" "blue")) DECK-REDBLUE)
(define (make-uno-deck limit all-colors)
  (local [;; add-color-cards : String [List-of UnoCard] -> [List-of UnoCard]
          ;; Add cards of the given color to the deck
          (define (add-color-cards color sofar)
            (append (generate-cards-of-color limit color) sofar))]
    (foldr add-color-cards '() all-colors)))

;; generate-cards-of-color : Nat String -> [List-of UnoCard]
;; Generate all cards of the given color that are less than the given number
(check-expect (generate-cards-of-color 0 "green") '())
(check-expect (generate-cards-of-color 2 "red") (list CARD-RED0 CARD-RED1))
(define (generate-cards-of-color limit color)
  (local [;; card-of-value : Nat -> UnoCard
          ;; Produce an UnoCard of the given value
          (define (card-of-value val)
            (make-card color val))]
    (build-list limit card-of-value)))

;; BECCA : To be honest, we really should have checked the properties of the list in make-uno-deck
;; instead of checking the list itself because we didn't know what order the cards would come out
;; in. But if you don't understand what I mean by that it's okay, you can just ignore this ramble.