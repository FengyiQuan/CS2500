;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab2-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

; A Shape is one of:
; - "circle"
; - "square"
; - "triangle"
; and represents a kind of shape

(define CIRCLE "circle")
(define SQUARE "square")
(define TRIANGLE "trangle")

; shape-temp : Shape -> ?
(define (shape-temp s)
  (cond [(string=? s CIRCLE) ...]
        [(string=? s SQUARE) ...]
        [(string=? s TRIANGLE) ...]))


(define CIRCLE-IMAGE (circle 15 "solid" "blue"))
(define SQUARE-IMAGE (square 15 "solid" "blue"))
(define TRIANGLE-IMAGE (triangle 15 "solid" "blue"))

;; draw : Shape -> Image
;; Draw the shape
(check-expect (draw CIRCLE) CIRCLE-IMAGE)
(check-expect (draw SQUARE) SQUARE-IMAGE)
(check-expect (draw TRIANGLE) TRIANGLE-IMAGE)
(define (draw s)
  (cond [(string=? s CIRCLE) CIRCLE-IMAGE]
        [(string=? s SQUARE) SQUARE-IMAGE]
        [(string=? s TRIANGLE) TRIANGLE-IMAGE]))

;; draw/scene : Shape -> Image
;; Draw the shape on a empty scene
(check-expect (draw/scene CIRCLE) (overlay CIRCLE-IMAGE (empty-scene 100 100)))
(define (draw/scene s)
  (overlay (draw s) (empty-scene 100 100)))
;; ^ does not need to follow the template as we want to draw every image
;;   and place it on the same shape. draw, our helper function, handles what to do in the case
;;   of each shape.
;; ^ only 1 test is needed, as we assume draw is well tested, and the code here only runs
;;   along 1 conditional branch/has no edge cases.

;; next-shape : Shape -> Shape
;; Produce the next shape
(check-expect (next-shape CIRCLE) SQUARE)
(check-expect (next-shape SQUARE) TRIANGLE)
(check-expect (next-shape TRIANGLE) CIRCLE)
(define (next-shape s)
  (cond [(string=? s CIRCLE) SQUARE]
        [(string=? s SQUARE) TRIANGLE]
        [(string=? s TRIANGLE) CIRCLE]))

;; main : Shape -> Shape
;; Run the shape animation with s as the first shape
(define (main s)
  (big-bang s
    [on-tick next-shape 1]
    [to-draw draw/scene]))

(define EASY-UP 5)
(define MEDIUM-UP 3)
(define HARD-UP 1)

(define HEIGHT 99)

;; A Terrain is one of:
;; - [0, 69]
;; - [70, 89]
;; - [90, 99]
;; and represents increasingly difficult areas of the mountain

(define TERRAIN-EASY 0)
(define TERRAIN-MEDIUM 71)
(define TERRAIN-HARD 91)

;; terrain-temp : Terrain -> ?
(define (terrain-temp t)
  (cond [(<= 0 t 69) ...]
        [(<= 70 t 89) ...]
        [(<= 90 t 99) ...]))

;; move-up : Terrain -> Terrain
;; Move up in the terrain
(check-expect (move-up HEIGHT) HEIGHT)
(check-expect (move-up TERRAIN-EASY) 5)
(check-expect (move-up TERRAIN-MEDIUM) 74)
(check-expect (move-up TERRAIN-HARD) 92)
(define (move-up t)
  (min HEIGHT
       (cond [(<= 0 t 70) (+ t EASY-UP)]
             [(<= 71 t 90) (+ t MEDIUM-UP)]
             [(<= 91 t 100) (+ t HARD-UP)])))

(define BG
  (above
   (rectangle 20 10  "solid" "brown")
   (rectangle 20 20  "solid" "orange")
   (rectangle 20 70  "solid" "yellow")))

(define CLIMBER (circle 5 "solid" "black"))

;; place-terrain : Terrain -> Image
;; Place the climber on the terrain at t
(check-expect (place-terrain TERRAIN-EASY) (place-image CLIMBER 10 HEIGHT BG))
(define (place-terrain t)
  (place-image CLIMBER 10 (- HEIGHT t) BG))

;; climb : Terrain KeyEvent -> Terrain
;; Move up if "up", else 0
(check-expect (climb TERRAIN-EASY "up") 5)
(check-expect (climb TERRAIN-EASY "down") 0)
(define (climb t ke)
  (cond [(key=? ke "up") (move-up t)]
        [else 0]))

;; reached-the-top? : Terrain -> Boolean
;; At the top of the terrain?
(check-expect (reached-the-top? 0) #f)
(check-expect (reached-the-top? HEIGHT) #t)
(define (reached-the-top? t)
  (= t HEIGHT))

;; main/climb : ? -> Terrain
;; Run the climber game
(define (main/climb _)
  (big-bang 0
    [on-key climb]
    [stop-when reached-the-top?]
    [to-draw place-terrain]))