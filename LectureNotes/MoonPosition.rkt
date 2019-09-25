;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname MoonPosition) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
; A MoonPosition is a (make-posn Number Number)
; interpretation : the x/y position of the moon
; Examples:
(define MOON-POSITION-1 (make-posn 1 1))
(define MOON-POSITION-2 (make-posn 20 30.8))

(define (moon-position-temp mp)
  (... (posn-x mp) ... (posn-y mp) ...))

;constant
(define SUN (circle 25 "solid" "yellow"))
(define MOON (circle 25 "solid" "grey"))
(define BACKGROUND (rectangle 300 200 "solid" "light blue"))
(define SKY (overlay SUN BACKGROUND))

; main : MoonPosition -> MoonPosition
; animates an eclipse with a moon mobing across the sky
(define (main initial-moon-pos)
  (big-bang initial-moon-pos
    [to-draw draw-eclipse]
    [on-tick move-moon]))

; draw-eclipse : MoonPosition -> image
; draws current state of the eclipse
(check-expect (draw-eclipse MOON-POSITION-1)
              (place-image MOON 1 1 SKY))
(check-expect (draw-eclipse MOON-POSITION-2)
              (place-image MOON 20 30.8 SKY))

(define (draw-eclipse mp)
  (place-image MOON (posn-x mp) (posn-y mp)SKY))

; move-moon : MoonPosition -> MoonPosition
; move the moon diagonally downward
(check-expect (move-moon MOON-POSITION-1)
              (make-posn 2 2))
(check-expect (move-moon MOON-POSITION-2)
              (make-posn 21 31.8))

(define (move-moon mp)
  (make-posn (+ 1 (posn-x mp)) (+ 1 (posn-y mp))))





