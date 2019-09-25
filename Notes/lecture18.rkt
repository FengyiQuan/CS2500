;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture18) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. You all should have NEW partners. Please ensure that you can submit with your new partner
;;    by Monday at 5pm or we cannot guarantee that you will be able to submit assignment 10. We
;;    are not going to provide any sort of extension if you can't submit on time as it is your
;;    responsibility to ensure you can submit by Monday at 5pm.
;; 2. If you missed lab for some reason you MUST contact your head TA to receive a new partner.
;; 3. Homework 9 is due tomorrow at 9pm. It seems like people are having a lot of trouble
;;    designing their data definition. There is a post on Piazza about this. It's a pinned post.
;;    Take a look at that post for additional help on designing data.

;; Let's practice list abstractions by making a simple little wheel of fortune game.

;; DATA DEFINITIONS

(define-struct guessed [char])
(define-struct unguessed [char])

;; A Game1String is one of:
;; - (make-guessed 1String)
;;   - where char is the letter that has been guessed and revealed
;; - (make-unguessed 1String)
;;   - where char is the letter the player needs to guess

(define GUESSED1 (make-guessed "a"))
(define GUESSED2 (make-guessed "p"))
(define UNGUESSED1 (make-unguessed "a"))
(define UNGUESSED2 (make-unguessed "p"))

;; game1string-temp : Game1String -> ???
(define (game1string-temp g1s)
  (cond [(guessed? g1s) (... (guessed-char g1s) ...)]
        [(unguessed? g1s) (... (unguessed-char g1s) ...)]))

;; A WheelOfFortune (WOF) is a [List-of Game1String]

(define WOF0 '())
(define WOF1 (list UNGUESSED1 UNGUESSED2))
(define WOF2 (list UNGUESSED1 GUESSED2))
(define WOF3 (list GUESSED1 UNGUESSED2))
(define WOF4 (list GUESSED1 GUESSED2))

;; wof-template : WOF -> ???
(define (wof-template wof)
  (cond [(empty? wof) ...]
        [(cons? wof)
         (... (game1string-temp (first wof))
              (wof-template (rest wof)) ...)]))

;; CONSTANTS
(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define BOX-SIZE (* FONT-SIZE 2))
(define OUTLINE-BOX-IMAGE
  (overlay (square BOX-SIZE "outline" "black") (square (+ BOX-SIZE 4) "solid" "white")))
(define UNGUESSED-IMAGE
  (overlay (text "?" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))

;; FUNCTIONALITY

;; guessing-game : String -> WOF
;; Play wheel of fortune with the given word
(define (guessing-game word)
  (big-bang (initial-world-state word)
    [to-draw draw-wof]
    [on-key guess-letter]
    [stop-when all-letters-revealed?]))

;; initial-world-state : String -> WOF
;; Produces a WOF for the given string where all letters are unguessed
(check-expect (initial-world-state "") '())
(check-expect (initial-world-state "ap") (list UNGUESSED1 UNGUESSED2))
(define (initial-world-state word)
  (map make-unguessed (explode word)))

;; BECCA: I changed the drawing function just a tad because I wanted to do everything in one
;; iteration as opposed to first converting the list to a list of strings and then appending
;; them together and then drawing them. You can also do it the way Professor Mislove did
;; and it is just as valid. I just think this way looks better when you're playing the game :)

;; draw-wof : WOF -> Image
;; Draw the given wheel of fortune game
(check-expect (draw-wof WOF0) empty-image)
(check-expect (draw-wof WOF2) (draw-beside UNGUESSED1 (draw-beside GUESSED2 empty-image)))
(define (draw-wof wof)
  (foldr draw-beside empty-image wof))

;; draw-beside : Game1String Image -> Image
;; Draw the given Game1String beside the given image
(check-expect
 (draw-beside GUESSED1 empty-image)
 (overlay (text "a" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(check-expect
 (draw-beside UNGUESSED2 (square 10 "solid" "red"))
 (beside UNGUESSED-IMAGE (square 10 "solid" "red")))
(define (draw-beside g1s img)
  (cond [(guessed? g1s)
         (beside (draw-guessed (guessed-char g1s)) img)]
        [(unguessed? g1s)
         (beside UNGUESSED-IMAGE img)]))

;; draw-guessed : 1String -> Image
;; Draw a guessed letter
(check-expect
 (draw-guessed "x")
 (overlay (text "x" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(check-expect
 (draw-guessed "a")
 (overlay (text "a" FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))
(define (draw-guessed letter)
  (overlay (text letter FONT-SIZE FONT-COLOR) OUTLINE-BOX-IMAGE))

;; BECCA: We didn't get a chance to implement the key handler function.
;; Try implementing this one on your own!

;; guess-letter : WOF KeyEvent -> WOF
;; Guess the given letter (if it is valid)
#;#;(check-expect (guess-letter WOF0 "x") '())
(check-expect (guess-letter WOF2 "a") WOF4)
(define (guess-letter wof ke) wof) ;; TO DO

;; all-letters-revealed? : WOF -> Boolean
;; Are all the letters in the word revealed?
(check-expect (all-letters-revealed? WOF0) #true)
(check-expect (all-letters-revealed? WOF2) #false)
(check-expect (all-letters-revealed? WOF4) #true)
(define (all-letters-revealed? wof)
  (andmap guessed? wof))

;; Let's talk a bit about andmap, which I've used above. We haven't discussed this list abstraction
;; before but it is on the list abstraction cards we handed out. Basically, andmap returns #true
;; if some test passes for every element in a list. Let's define our own to see how this works.

;; is-true-for-all? : (X) [X -> Boolean] [List-of X] -> Boolean
;; Does every element in the list pass the test?
(check-expect (is-true-for-all? string? '()) #true)
(check-expect (is-true-for-all? number? '(1 2 3)) #true)
(check-expect (is-true-for-all? even? '(2 3 4)) #false)
(define (is-true-for-all? test lox)
  (cond [(empty? lox) #true]
        [(cons? lox) (and (test (first lox))
                          (is-true-for-all? test (rest lox)))]))

;; It turns out there is a similar function called 'ormap' which returns true if something is true
;; for at least ONE element (as opposed to ALL elements).
;; BECCA: Just for kicks and giggles I've defined ormap for you as well.

;; is-true-for-any? : (X) [X -> Boolean] [List-of X] -> Boolean
;; Does any element in the list pass the test?
(check-expect (is-true-for-any? string? '()) #false)
(check-expect (is-true-for-any? number? (list #true 1 "hello")) #true)
(check-expect (is-true-for-any? even? (list 1 3 5 7 9)) #false)
(define (is-true-for-any? test lox)
  (cond [(empty? lox) #false]
        [(cons? lox) (or (test (first lox))
                         (is-true-for-any? test (rest lox)))]))