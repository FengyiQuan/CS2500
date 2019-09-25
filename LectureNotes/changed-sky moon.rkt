;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |changed-sky moon|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define SKY-WIDTH 600)
(define SKY-HEIGHT 400)
(define SUN (circle 25 "solid" "yellow"))
(define MOON (circle 25 "solid" "gray"))
(define SKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define DARKSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "dark blue"))
(define DIMSKY (rectangle SKY-WIDTH SKY-HEIGHT "solid" "blue"))

;draw-eclipse : num -> image
;draw the moon at the given x-coordinate, on a scene with the sun
(define (draw-eclipse x-moon)
  (cond
    [(= x-moon (/ SKY-WIDTH 2))
        (place-image MOON
                     x-moon (/ SKY-HEIGHT 2)
                     (place-image SUN
                                  (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) DARKSKY))]
     [(< (abs (- x-moon (/ SKY-WIDTH 2))) 50)
                 (place-image MOON
                              x-moon (/ SKY-HEIGHT 2)
                              (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) DIMSKY))]
     [else (place-image MOON
                        x-moon (/ SKY-HEIGHT 2)
                        (place-image SUN
                                     (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2)
                                     SKY))]))
