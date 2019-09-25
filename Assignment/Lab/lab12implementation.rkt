;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname lab12-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; biggest-transformation : [Num -> Num] [Num -> Num] -> [Num -> Num]
;; Return the larger of the transformations
(check-expect ((biggest-transformation add1 sqr) -2) 4)
(define (biggest-transformation f g)
  (λ (num) (max (g num) (f num))))

; A Ball is a (make-ball Nat Mode Color [Nat -> Posn])
(define-struct ball [r mode color placement])
; INTERPRETATION:
; - r is the ball's radius
; - mode is the ball's mode
; - color is the ball's color
; - placement is a function that, given the current time,
;   outputs a new coordinate for the ball to be drawn at

; A Mode is one of:
; - "solid"
; - "outline"

(define BALL-1 (make-ball 3 "solid" "red" (λ (x) (make-posn 5 x))))
(define BALL-2 (make-ball 3 "solid" "blue" (λ (x) (make-posn (+ x 2) 3))))

; A BallWorld is a (make-world Nat [List-of Ball])
(define-struct ball-world [t balls])
; - where t is the amount of time that has passed
; - and balls is the balls of the world

(define WORLD-1 (make-ball-world 0 '()))
(define WORLD-2 (make-ball-world 10 (list BALL-1 BALL-2)))



; main : [List-of Ball] -> World
; Run the game with this list of initial balls
(define (main init-list)
  (big-bang (make-ball-world 0 init-list)
            [on-tick (λ (bw) (make-ball-world (add1 (ball-world-t bw))
                                              (ball-world-balls bw)))]
            [to-draw draw]
            [on-mouse place-ball]))

(define WIDTH 500)
(define HEIGHT 500)
(define BG (empty-scene WIDTH HEIGHT))

;; draw-ball : Ball Posn Image -> Image
;; Draw b on i at p
(check-expect (draw-ball BALL-1 (make-posn 0 0) BG)
              (place-image (circle 3 "solid" "red") 0 0 BG))
(define (draw-ball b p i)
  (place-image (circle (ball-r b) (ball-mode b) (ball-color b))
               (posn-x p) (posn-y p)
               i))

;; make-drawer : Nat -> [Ball Image -> Image]
;; Make a function that will draw a ball
(check-expect ((make-drawer 0) BALL-1 BG)
              (place-image (circle 3 "solid" "red") 5 0 BG))
(define (make-drawer t)
  (λ (b i) (draw-ball b ((ball-placement b) t) i)))

;; draw : BallWorld -> Image
;; Draw the ball world
(check-expect (draw WORLD-2)
              (place-image (circle 3 "solid" "blue") 12 3
                           (place-image (circle 3 "solid" "red") 5 10 BG)))
(define (draw bw)
  (foldr (make-drawer (ball-world-t bw)) BG (ball-world-balls bw)))

; A BallGenerator is a [Nat Nat Nat -> [Nat -> Posn]]
; Given the time, x-coordinate, and y-coordinate of when and where a
; ball is created, create a function that, given the current time of
; the world, will output a Posn

; Example:
; move-horizontally : BallGenerator
(define (move-horizontally t0 x0 y0)
  (λ (t) (make-posn (modulo (+ x0 (- t t0)) WIDTH) y0)))
(check-expect ((move-horizontally 3 5 8) 10) ; 7 seconds have passed
              (make-posn 12 8))

; move-horizontally : BallGenerator
(define (move-vertically t0 x0 y0)
  (λ (t) (make-posn x0 (modulo (+ y0 (- t t0)) HEIGHT))))
(check-expect ((move-vertically 3 5 8) 10) ; 7 seconds have passed
              (make-posn 5 15))

(define GENERATORS (list move-horizontally move-vertically))

;; place-ball : BallWorld Number Number MouseEvent -> BallWorld
;; Place the ball when someone clicks
(check-expect (place-ball WORLD-1 0 0 "button-up") WORLD-1)
(check-expect (length (ball-world-balls (place-ball WORLD-1 0 0 "drag"))) 1)
(define (place-ball bw x y me)
  (if (not (mouse=? me "drag"))
      bw
      (make-ball-world  (ball-world-t bw)
                        (cons (make-ball (+ 2 (random 10))
                                         "solid"
                                         (make-color (random 256) (random 256) (random 256))
                                         ((select-random GENERATORS) (ball-world-t bw) x y))
                              (ball-world-balls bw)))))

;; select-random : [List-of X] -> X
;; select randomly from the list
(check-expect (select-random (list "a")) "a")
(define (select-random lox)
  (list-ref lox (random (length lox))))


; A Repeater is a function [[X -> X] -> [X -> X]]
; That, given a one-argument function f, outputs a
; function that will repeatedly apply f some specific number of times

;; two : Repeater
(check-expect ((two add1) 0) 2)
(define (two f) (λ (x) (f (f x))))

;; three : Repeater
(check-expect ((three add1) 0) 3)
(define (three f) (λ (x) (f (f (f x)))))

;; one : Repeater
(check-expect ((one add1) 0) 1)
(define one identity)

;; zero : Repeater
(check-expect ((zero add1) 0) 0)
(define (zero f) (λ (x) x))

;; rep->nat : Repeater -> Nat
;; How many times does this repeater repeat
(check-expect (rep->nat zero) 0)
(check-expect (rep->nat one) 1)
(check-expect (rep->nat two) 2)
(check-expect (rep->nat three) 3)
(check-expect (rep->nat (λ (f) (λ (x) ((three f) ((two f) x))))) 5)
(define (rep->nat r) ((r add1) 0))

;; rep-add1 : Repeater -> Repeater
;; Repeat one more time
(check-expect (rep->nat (rep-add1 two)) 3)
(define (rep-add1 r)
  (λ (f) (λ (x) (f ((r f) x)))))

;; nat->rep : Nat -> Repeater
;; A repeater that repeats this many times
(check-expect (rep->nat (nat->rep 3)) 3)
(define (nat->rep n)
  (if (zero? n) zero (rep-add1 (nat->rep (sub1 n)))))

;; rep+ : Repeater Repeater -> Repeater
;; Sum the repeaters
(check-expect (rep->nat (rep+ two three)) 5)
(define (rep+ r1 r2)
  (λ (f) (λ (x) ((r2 f) ((r1 f) x)))))

;; rep* : Repeater Repeater -> Repeater
;; Multiply the repeaters
(check-expect (rep->nat (rep* two three)) 6)
(define (rep* r1 r2)
  (λ (f) (λ (x) ((r2 (r1 f)) x))))

;; rep^ : Repeater Repeater -> Repeater
;; r1^r2
(check-expect (rep->nat (rep^ two three)) 8)
(define (rep^ r1 r2)
  (λ (f) (λ (x) (((r2 r1) f) x))))