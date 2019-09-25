;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab1-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; multiple-of-5? : Number -> Boolean
;; Is x a multiple of 5?
(check-expect (multiple-of-5? 10) #t)
(check-expect (multiple-of-5? -10) #t)
(check-expect (multiple-of-5? 13) #f)
(define (multiple-of-5? x)
  (integer? (/ x 5)))

(define GREETING "I will always love you")

;; greet : String -> String
;; Greet name
(check-expect (greet "Matt") "I will always love you, Matt")
(define (greet s)
  (string-append GREETING ", " s))

;; times-itself : Number -> Number
;; Multiply a number by itself
(check-expect (times-itself 0) 0)
(check-expect (times-itself 2) 4)
(define (times-itself n)
  (+ n n))

;; The above is incorrect; + should be *. People rely on code to do what it says it will do.

;; percent/flint : Number -> Number
;; What percentage of this net worth would it take to fix the water in Flint, Michigan?
(check-expect (percent/flint 19700000000) 55000000/19700000000)
(check-expect (percent/flint 140900000000) 55000000/140900000000)
(define (percent/flint x)
  (/ 55000000 x))

; A Score is a number in the range [0, 100]

;; letter-grade : Score -> String
;; The letter grade of a score
(check-expect (letter-grade 90) "A")
(check-expect (letter-grade 80) "B")
(check-expect (letter-grade 70) "C")
(check-expect (letter-grade 65) "D")
(check-expect (letter-grade 64) "F")
(define (letter-grade score)
  (cond [(>= score 90) "A"]
        [(>= score 80) "B"]
        [(>= score 70) "C"]
        [(>= score 65) "D"]
        [(>= score 00) "F"]))

;; wage : Number Number -> Number
;; The amount owed when h hours are worked at d dollars per hour,
;; any hours over 40 paid at 1.5*d
(check-expect (wage 4 3) 12)
(check-expect (wage 43 3) 133.5)
(define (wage h d)
  (cond [(<= h 40) (* h d)]
        [else (+ (* (- h 40) d 1.5)
                 (* 40 d))]))

;; runtime : String -> Number
;; The number of minutes in the given film
(check-expect (runtime "Black Swan") 110)
(check-expect (runtime "Requiem For A Dream") 102)
(check-error (runtime "Noah"))
(define (runtime film)
  (cond [(string=? film "Black Swan") 110]
        [(string=? film "Requiem For A Dream") 102]
        [else (error)]))

(require 2htdp/image)

(define ROOF (triangle 150 "solid" "black"))
(define BODY (square 150 "solid" "red"))
(define DOOR-FRAME (rectangle 25 75 "solid" "white"))
(define DOOR-HANDLE (circle 5 "solid" "brown"))
(define DOOR (overlay/align "left" "center"
                            DOOR-HANDLE
                            DOOR-FRAME))
(define BODY-WITH-DOOR (overlay/align "center" "bottom"
                                      DOOR
                                      BODY))
(define HOUSE (above ROOF BODY-WITH-DOOR))

(define WINDOW (square 50 "outline" "white"))

(define HOUSE-WITH-WINDOWS
  (place-image WINDOW 40 175
               (place-image WINDOW
                            110 175
                            HOUSE)))

;; scene : Number -> Image
;; Draw a circle of radius r mod 20 on a 50x50 empty scene
(check-expect (scene 1)
              (overlay (circle 1 "solid" "red") (empty-scene 50 50)))
(define (scene r)
  #;(overlay (circle (modulo r 20) "solid" "red") (empty-scene 50 50))
  (overlay (circle (radius r) "solid" "red") (empty-scene 50 50)))

;; radius : Number -> Number
;; Compute the radius for the scene at this time
(check-expect (radius 19) 19)
(check-expect (radius 20) 20)
(check-expect (radius 21) 19)
(check-expect (radius 39) 1)
(check-expect (radius 40) 0)
(check-expect (radius 41) 1)
(define (radius r)
  (cond [(even? (quotient r 20)) (modulo r 20)]
        [else (- 20 (modulo r 20))]))
(animate scene)