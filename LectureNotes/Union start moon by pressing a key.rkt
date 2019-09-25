;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Union start moon by pressing a key|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A MoonPosition is one of:
; - "waiting"
; - RealNumber
; Interpretation: waiting for a keypress or, if started, the location of the moon on the x-axis
; Examples:
(define MOONPOSITION-1 0)
(define MOONPOSITION-WAITING "waiting")

#;
(define (moonposition -temp mp)
  (cond
    [(string? mp) ...]
    [(number? mp) ... mp ...]))

(define SUN (circle 25 "solid" "yellow"))
(define MOON (circle 25 "solid" "gray"))
(define SKY (rectangle 300 200 "solid" "light blue"))
(define SUN-X 220)
(define SUN-Y 150)

; eclipse : MoonPosition -> MoonPosition
; Runs an eclipse animation
(define (eclipse initial-position)
  (big-bang initial-position
    [to-draw draw-eclipse]
    [on-tick move-moon]
    [on-key start-eclipse]))

; move-eclipse : MoonPosition -> MoonPosition
; Moves the moon
(check-expect (move-moon 0) 1)
(check-expect (move-moon 10) 11)
(check-expect (move-moon MOONPOSITION-WAITING) MOONPOSITION-WAITING)

(define (move-moon mp)
  (cond
    [(number? mp) (+ mp 1)]
    [(string? mp) mp]))

; draw-eclipse :MoonPosition -> Image
; Draw the moon at the given x-coordinate, on a scene with the sun

(check-expect (draw-eclipse 10) (place-image MOON 10 SUN-Y
                                             (place-image SUN SUN-X SUN-Y SKY)))
(check-expect (draw-eclipse  MOONPOSITION-WAITING) 
                                             (place-image SUN SUN-X SUN-Y SKY))

(define (draw-eclipse mp)
  (cond
    [(string? mp) (place-image SUN SUN-X SUN-Y SKY)]
    [(number? mp) (place-image MOON mp SUN-Y
                               (place-image SUN SUN-X SUN-Y SKY))]))

; start-eclipse : MoonPosition KeyEvent -> MoonPosition
; if waiting, starts the eclipse; otherwise does nothing

(check-expect (start-eclipse MOONPOSITION-1 "right")
              MOONPOSITION-1)
;(check-expect (start-eclipse MOONPOSITION-2 "right")
 ;             MOONPOSITION-2)
(check-expect (start-eclipse MOONPOSITION-WAITING "right")
              0)

(define (start-eclipse mp ke)
  (cond
    [(string? mp) 0]
    [(number? mp) mp]))














