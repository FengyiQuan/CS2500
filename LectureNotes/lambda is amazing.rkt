;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |lambda is amazing|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
((λ (x)
   (big-bang (make-posn (map (λ (x) (+ 32 (sqrt (+ 2500 x)))) x) 0)
     [on-tick (λ (x) (if (< (posn-y x) 11)
                         (make-posn (posn-x x) (add1 (posn-y x)))
                         (make-posn (posn-x x) ((λ (x) (- x 10)) (posn-y x)))))]
     [to-draw (λ (x)
                (overlay
                 (text (foldr (λ (x y) (string-append (string (integer->char x)) y)) "" (posn-x x))
                       100
                       (list-ref (list (make-color 255 0 0) (make-color 255 127 0)
                                       (make-color 255 255 0) (make-color 127 255 0)
                                       (make-color 0 255 0) (make-color 0 255 127)
                                       (make-color 0 255 255) (make-color 0 127 255)
                                       (make-color 0 0 255) (make-color 127 0 255)
                                       (make-color 255 0 255) (make-color 255 0 127))
                                 (posn-y x)))
                 (empty-scene 1000 120)))]))
 (list 849429 -2500 2829 4389 -2500 1725 3429 1725 5600 2829 3584 2541 -2499))

