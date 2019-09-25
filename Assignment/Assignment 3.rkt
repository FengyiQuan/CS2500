;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 1
;; Exercise 1
(require 2htdp/image)
(require 2htdp/universe)
;; Examples:
;; A CryptoName is one of:
;; - "Alice"
;; - "Bob"
;; - "Carol"
;; and represents the collection of names used in crypotography narratives
(define CryptoName-Alice "Alice")
(define CryptoName-Bob "Bob")
(define CryptoName-Carol "Carol")

;; Exercise 2
(define (CryptoName-temp c)
  (cond
    [... CryptoName-Alice]
    [... CryptoName-Bob]
    [... CryptoName-Carol]))

;; Exercise 3
;; CryptoName : KeyEvent -> string
;; takes a letter and returns its cryptoname
(check-expect (CryptoName "a") "Alice")
(check-expect (CryptoName "A") "Alice")
(check-expect (CryptoName "b") "Bob")
(check-expect (CryptoName "B") "Bob")
(check-expect (CryptoName "c") "Carol")
(check-expect (CryptoName "C") "Carol")
(define (CryptoName ke)
  (cond
    [(key=? ke "a") CryptoName-Alice]
    [(key=? ke "A") CryptoName-Alice]
    [(key=? ke "b") CryptoName-Bob]
    [(key=? ke "B") CryptoName-Bob]
    [(key=? ke "c") CryptoName-Carol]
    [(key=? ke "C") CryptoName-Carol]))

;; Exercise 4
;; right-arrow : KeyEvent -> CryptoName
;; outputs the next CryptoName by pressing "right"
(check-expect (right-arrow CryptoName-Alice "right") CryptoName-Bob)
(check-expect (right-arrow CryptoName-Bob "right") CryptoName-Carol)
(check-expect (right-arrow CryptoName-Carol "right") CryptoName-Alice)
(check-expect (right-arrow CryptoName-Bob "a") CryptoName-Alice)
(check-expect (right-arrow CryptoName-Bob "A") CryptoName-Alice)
(check-expect (right-arrow CryptoName-Alice "b") CryptoName-Bob)
(check-expect (right-arrow CryptoName-Alice "c") CryptoName-Carol)
(check-expect (right-arrow CryptoName-Alice "r") "backup")
(define (right-arrow current-text ke)
  (cond
    [(and (key=? ke "right") (string=? current-text CryptoName-Alice)) CryptoName-Bob]
    [(and (key=? ke "right") (string=? current-text CryptoName-Bob)) CryptoName-Carol]
    [(and (key=? ke "right") (string=? current-text CryptoName-Carol)) CryptoName-Alice]
    [(key=? ke "a") CryptoName-Alice]
    [(key=? ke "A") CryptoName-Alice]
    [(key=? ke "b") CryptoName-Bob]
    [(key=? ke "B") CryptoName-Bob]
    [(key=? ke "c") CryptoName-Carol]
    [(key=? ke "C") CryptoName-Carol]
    [else "backup"]))

;; Exercise 5
;; draw-name : CryptoName -> Image
;; draws CryptoName
(check-expect (draw-name CryptoName-Alice)
              (overlay (text CryptoName-Alice TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (draw-name CryptoName-Bob)
              (overlay (text CryptoName-Bob TEXT-SIZE TEXT-COLOR) BACKGROUND))
(check-expect (draw-name CryptoName-Carol )
              (overlay (text CryptoName-Carol TEXT-SIZE TEXT-COLOR) BACKGROUND))
(define WIDTH 400)
(define HEIGHT 200)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 50)
(define TEXT-COLOR "black")

(define (draw-name CryptoName)
  (cond
    [(string=? CryptoName CryptoName-Alice)
     (overlay (text CryptoName-Alice TEXT-SIZE TEXT-COLOR) BACKGROUND)]
    [(string=? CryptoName CryptoName-Bob)
     (overlay (text CryptoName-Bob TEXT-SIZE TEXT-COLOR) BACKGROUND)]
    [(string=? CryptoName CryptoName-Carol)
     (overlay (text CryptoName-Carol TEXT-SIZE TEXT-COLOR) BACKGROUND)]
    [else (overlay (text "backup" TEXT-SIZE TEXT-COLOR) BACKGROUND)]))

;; Exercise 6
;; main/crypto : World -> World
;; to show certain CryptoName by pressing given key
(define (main/crypto CryptoName)
  (big-bang CryptoName
    [to-draw draw-name]
    [on-key right-arrow]))
