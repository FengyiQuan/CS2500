;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;lab 2
;Exercise (Reviewed) 1 
(require 2htdp/image)
(require 2htdp/universe)
; A shape is one of:
; - "circle"
; - "square"
; - "triangle"
; and represents a kind of shape

(define SQUARE "square")
(define CIRCLE "circle")
(define TRIANGLE "triangle")

(define (shape-temp a-shape)
  (cond
    [(string=? a-shape SQUARE) ...]
    [(string=? a-shape CIRCLE) ...]
    [(string=? a-shape TRIANGLE) ...]))

; design function draw which draws a Shape
; draw-shape : shape -> image
; draw a shape by given a name of a Shape
(check-expect (draw-shape CIRCLE) (circle 45 "solid" "gray"))
(check-expect (draw-shape SQUARE) (square 45 "solid" "gray"))
(check-expect (draw-shape TRIANGLE) (triangle 45 "solid" "gray"))

(define (draw-shape a-shape)
  (cond
    [(string=? a-shape SQUARE) (square 45 "solid" "gray") ]
    [(string=? a-shape CIRCLE) (circle 45 "solid" "gray")]
    [(string=? a-shape TRIANGLE) (triangle 45 "solid" "gray")]))

;exercise 3
;draw/scene : shape -> image
; draw a shape in an empty-scene
(check-expect (draw/scene SQUARE)(overlay (square 45 "solid" "gray") (empty-scene 45 45)))
(check-expect (draw/scene CIRCLE)(overlay (circle 45 "solid" "gray") (empty-scene 45 45)))
(check-expect (draw/scene TRIANGLE)(overlay (triangle 45 "solid" "gray") (empty-scene 45 45)))
(define (draw/scene a-shape)
  (cond
    [(string=? a-shape SQUARE)(overlay (square 45 "solid" "gray") (empty-scene 45 45))]
    [(string=? a-shape CIRCLE)(overlay (circle 45 "solid" "gray") (empty-scene 45 45))]
    [(string=? a-shape TRIANGLE)(overlay (triangle 45 "solid" "gray") (empty-scene 45 45))]))

;exercise 4
;next-shape : shape -> shape
;outputs the "next" shape
(check-expect (next-shape SQUARE) CIRCLE)
(check-expect (next-shape CIRCLE) TRIANGLE)
(check-expect (next-shape TRIANGLE) SQUARE)
         
(define (next-shape a-shape)
  (cond
    [(string=? a-shape SQUARE) CIRCLE]
    [(string=? a-shape CIRCLE) TRIANGLE]
    [(string=? a-shape TRIANGLE) SQUARE]))
    
;exercise 5
;shape-v1 : world -> image
;draw shape with an certain order
(define (shape-v1 a-shape)
  (big-bang a-shape
    [to-draw draw/scene]
    [on-tick next-shape]))

;exercise 6
;shape-v2 : world -> image
;draw shape with an certain order
(define (shape-v2 a-shape)
  (big-bang a-shape
    [to-draw draw/scene]
    [on-tick next-shape 1/2]))
;exercise 7
;main : world -> image
;takes a Shape and uses that shape as the initial state
(define (main a-shape)
  (cond
    [(string=? a-shape SQUARE)(big-bang a-shape
                        [to-draw draw/scene]
                        [on-tick next-shape 1/2])]
    [(string=? a-shape CIRCLE) (big-bang a-shape
                         [to-draw draw/scene]
                         [on-tick next-shape 1/2])]
    [(string=? a-shape TRIANGLE)(big-bang a-shape
                          [to-draw draw/scene]
                          [on-tick next-shape 1/2])]))

;exercise 9
;a constant is one of :
; - "EASY-UP"
; - "MEDIUM-UP"
; - "HARD-UP"
;to represent how many pixels our brave climber can move in a single movement
;in terrains of varying climbing difficulty
(define constant1 "EASY-UP")
(define constant2 "MEDIUM-UP")
(define constant3 "HARD-UP")
(define HEIGHT 400)

;exercise 10
;Terrain "EASY-UP" is in [0, 200]
;"MEDIUM-UP" is in (200, 300]
;"HARD-UP" is in (300, 400]

;exercise 11
(define someone (circle 20 "solid" "blue"))
(define mountain (rectangle 600 400 "outline" "brown"))
(define (place-terrain height)
  (place-image someone 300 height mountain))

;exercise 12
(define (climb height KeyEvent)
  (if (key=? KeyEvent "up") move-up
      0))

;exercise 13

;exercise 14
(define (main/climb Ke
   (big-bang Ke
     []
     []
     [])))
