;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname FortuneWheel) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; DATA DEFINITIONS
(define-struct guessed [char])
(define-struct unguessed [char])
;; A Game1String is one of:
;; - (make-guessed 1String)
;;   - where char is the letter that has been guessed and revealed
;; - (make-unguessed 1String)
;;   - where char is the letter the player needs to guess
;; Examples:
(define GUESSED1 (make-guessed "a"))
(define GUESSED2 (make-guessed "p"))
(define UNGUESSED1 (make-unguessed "a"))
(define UNGUESSED2 (make-unguessed "p"))
;; game1string-temp : Game1String -> ???
(define (game1string-temp g1s)
  (cond [(guessed? g1s) (... (guessed-char g1s) ...)]
        [(unguessed? g1s) (... (unguessed-char g1s) ...)]))

;; A WheelOfFortune (WOF) is a [List-of Game1String]
;; Examples:
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

;; guessing-game : String -> WOF
;; Play wheel of fortune with the given word
(define (guessing-game s)
  (big-bang (build-wof s)
    [to-draw draw-wof]
    [on-key guess-letter]
    [stop-when  all-letters-revealed?]))

;; build-wof : String -> WOF
;; Produces a WOF for the given string where all letters are unguessed
(check-expect (build-wof "adsf") (list (make-unguessed "a")
                                       (make-unguessed "d")
                                       (make-unguessed "s")
                                       (make-unguessed "f")))
(define (build-wof s)
  (map make-unguessed (explode s)))

;; all-letters-revealed? : WOF -> Boolean
;; Are all the letters in the word revealed?
(check-expect (all-letters-revealed? WOF0) #true)
(check-expect (all-letters-revealed? WOF2) #false)
(check-expect (all-letters-revealed? WOF4) #true)
(define (all-letters-revealed? wof)
  (andmap guessed? wof))

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

;; guess-letter : WOF KeyEvent -> WOF
;; Reveal all the letters that match the given key
(check-expect (guess-letter WOF0 "x") '())
(check-expect (guess-letter WOF2 "a") WOF4)
(define (guess-letter wof ke)
  (cond [(empty? wof) '()]
        [(cons? wof) (cons (reveal-if-match (first wof) ke)
                           (guess-letter (rest wof) ke))]))
;; reveal-if-match : Game1String KeyEvent -> Game1String
;; If the letter matches the given key, reveal it, otherwise leave it the same
(check-expect (reveal-if-match UNGUESSED1 "a") GUESSED1)
(check-expect (reveal-if-match UNGUESSED2 "b") UNGUESSED2)
(check-expect (reveal-if-match GUESSED2 "x") GUESSED2)
(define (reveal-if-match g1s ke)
  (cond [(guessed? g1s) g1s]
        [(unguessed? g1s) (if (string=? ke (unguessed-char g1s))
                              (make-guessed (unguessed-char g1s))
                              g1s)]))

;; guess-letter.v2 : WOF KeyEvent -> WOF
;; Reveal all the letters that match the given key
(check-expect (guess-letter.v2 WOF0 "x") '())
(check-expect (guess-letter.v2 WOF2 "a") WOF4)
(define (guess-letter.v2 wof ke)
  (local [;; guess-single-letter : Game1String -> Game1String
          ;; Reveal the letter if it matches the key that was pressed
          (define (guess-single-letter g1s)
            (reveal-if-match g1s ke))]
    (map guess-single-letter wof)))




































