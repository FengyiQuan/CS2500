;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Sky with a sun and a moon|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;constants
(define SKY-WIDTH 400)
(define SKY-HEIGHT 200)

(define SUN (circle 30 "solid" "yellow"))
(define MOON (circle 25 "solid" "white"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))

;draw-moon : number->image
;drqw the moon at the given x_coordinate
(define (draw-moon x-moon)
  (place-image MOON
               x-moon (/ SKY-HEIGHT 2)
               (place-image SUN
                            (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                            SKY)))

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

(animate draw-moon)