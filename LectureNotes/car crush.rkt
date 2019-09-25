;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |car crush|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct 2cars [pos1 pos2 vel1 vel2])
; A 2Cars is a (make-2cars num num num num)
; Interpretation: velocity and position of two cars
; - pos1 is the x coordinate of the first car in pixels from the left
; - pos2 is the x coordinate  of the second car in pixels from the left
; - vel1 is the velocity of the first car in pixels/ticks, + = going right
; - vel2 is the velocity of the second car in pixels/ticks, + = going right
; Examples:
(define 2CARS-1 (make-2cars 100 200 10 -5))

#;
(define (2cars-temp 2c)
  ( ... (2cars-pos1) ... (2cars-pos2) ... (2cars-vel1) ... (2cars-vel2) ... ))

; main: : 2Cars -> 2Cars
; simulation of two cars colliding
(define (main initial-world)
  (big-bang initial-world
    [to-draw draw-cars]
    [on-tick move-cars]
    [stop-when cars-off-screen? game-over]))

; draw-cars : 2Cars -> Image
; draw the two cars!

(define CAR1 (rectangle 100 50 "solid" "blue"))
(define CAR2 (rectangle 100 50 "solid" "pink"))
(define SCENE-WIDTH 600)
(define SCENE-HEIGHT 200)
(define CAR-Y (/ SCENE-HEIGHT 2))
(define BACKGROUND (rectangle SCENE-WIDTH SCENE-HEIGHT "solid" "white"))
(define ACCELERATION 0.5)

(check-expect (draw-cars 2CARS-1)
              (place-image
               CAR1 100 CAR-Y
               (place-image
                CAR2 200 CAR-Y
                BACKGROUND)))

(define (draw-cars 2c)
  (place-image
   CAR1 (2cars-pos1 2c) CAR-Y
   (place-image
    CAR2 (2cars-pos2 2c) CAR-Y
    BACKGROUND)))

; move-cars : 2Cars -> 2Cars
; move the two cars based upon their positions/velocities/acceleration
(check-expect (move-cars 2CARS-1)
              (make-2cars 110 195 10.5 -5.5))

(define (move-cars 2c)
  (make-2cars
   (+ (2cars-pos1 2c) (2cars-vel1 2c))
   (+ (2cars-pos2 2c) (2cars-vel2 2c))
   (+ (2cars-vel1 2c) ACCELERATION)
   (- (2cars-vel2 2c) ACCELERATION)))

; cars-off-screen? : 2Cars -> Boolean
; returns true if either car is off the screen

(check-expect (cars-off-screen? 2CARS-1) #false)
(check-expect (cars-off-screen? (make-2cars 1000000 200 1 -1)) #true)
(check-expect (cars-off-screen? (make-2cars 200 -1 1 -1)) #true)

(define (cars-off-screen? 2c)
  (or
   (> (2cars-pos1 2c) SCENE-WIDTH)
   (< (2cars-pos2 2c) 0)))

; game-over : World -> Image
(define (game-over w)
  (overlay
   (text "Game Over" 32 "red")
   BACKGROUND))
