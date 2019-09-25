;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Animate Sunset|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;constants
(define SKY-WIDTH 400)
(define SKY-HEIGHT 200)

(define SUN (circle 30 "solid" "yellow"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))

;function
(define (draw-sky y-position)
  (place-image
   SUN
   (/ SKY-WIDTH 2) y-position
   SKY))

(define (sky2 y-position)
  (cond
    ((>= y-position 30)
     (place-image SUN
                  (/ SKY-WIDTH 2) y-position
                  SKY))
     ((< y-position 30)
     (place-image SUN
                  (/ SKY-WIDTH 2) 30
                  SKY))))

;;Totally New animate
;properties of the "world" and the descending rocket

;constants
(define WIDTH 100)
(define HEIGHT 60)
(define V 3)
;the reason why V is 3 here because we consider 3 pixels per clock tick a good velocity.
(define X 50)

;graphical constants
(define MTSCN (empty-scene WIDTH HEIGHT))
(define ROCKET
  (rectangle 3 50 "solid" "dark blue"))
(define ROCKET-CENTER-TO-TOP
  (- HEIGHT (/ (image-height ROCKET) 2)))

;functions
(define (picture-of-rocket.v8 t)
  (cond
    ((<= (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET
                 X (distance t)
                 MTSCN))
    ((> (distance t) ROCKET-CENTER-TO-TOP)
     (place-image ROCKET
                  X ROCKET-CENTER-TO-TOP
                  MTSCN))))

(define (distance t)
  (* V t))

(animate picture-of-rocket.v8)