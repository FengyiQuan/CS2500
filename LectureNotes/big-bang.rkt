;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname big-bang) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;a light is one of:
; - "red"
; - "yellow"
; - "greed"
;interpretation: colors of a traffic light
;examples:
(define LIGHT-GREEN "green")
(define LIGHT-YELLOW "yellow")
(define LIGHT-RED "red")
(define LIGHT-RADIUS 25)

(define (light-temp l)
  (cond
    [(string=? l LIGHT-GREEN) ...]
    [(string=? l LIGHT-YELLOW) ...]
    [(string=? l LIGHT-RED) ...]))

;main : world -> world
; a world is a Light
(define (main initial-world)
 (big-bang initial-world
  [to-draw draw-traffic]
  [on-tick tick-traffic 1]))
  ;[on-key ]))

;draw-traffic : light -> image
;draws a traffic light given a color

(check-expect (draw-traffic LIGHT-GREEN)
              (circle LIGHT-RADIUS "solid" LIGHT-GREEN))
(check-expect (draw-traffic LIGHT-YELLOW)
              (circle LIGHT-RADIUS "solid" LIGHT-YELLOW))
(check-expect (draw-traffic LIGHT-RED)
              (circle LIGHT-RADIUS "solid" LIGHT-RED))
(define (draw-traffic l)
  (cond
    [(string=? l LIGHT-GREEN) 
              (circle LIGHT-RADIUS "solid" LIGHT-GREEN)]
    [(string=? l LIGHT-YELLOW) (circle LIGHT-RADIUS "solid" LIGHT-YELLOW)]
    [(string=? l LIGHT-RED) (circle LIGHT-RADIUS "solid" LIGHT-RED)]))

;tick-traffic : light ->light
;cycles a traffic light

(check-expect (tick-traffic  LIGHT-GREEN)  LIGHT-YELLOW)
(check-expect (tick-traffic  LIGHT-YELLOW)  LIGHT-RED)
(check-expect (tick-traffic  LIGHT-RED)  LIGHT-GREEN)
(define (tick-traffic l)
  (cond
    [(string=? l LIGHT-GREEN) LIGHT-YELLOW]
    [(string=? l LIGHT-YELLOW) LIGHT-RED]
    [(string=? l LIGHT-RED) LIGHT-GREEN]))



