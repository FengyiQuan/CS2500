;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname |snake game|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; Play Snake

;; constants:
(define BG (empty-scene 500 500))
(define SINGLE-SIZE 10)
(define SINGLE-BODY (square SINGLE-SIZE "solid" "light blue"))
(define APPLE (circle SINGLE-SIZE "solid" "red"))
(define TEXT-SIZE 30)
(define TEXT-COLOR "red")
(define Final (above (text "You die!!" 30 "red")
                     (text "Grade : " 30 "red")))

;; Direction is one of:
;; - "up"
;; - "down"
;; - "left"
;; - "right"
;; represents the four arrow keys

(define-struct snake [body direction apple])
;; Snake is a (make-snake [List-of Posn] String Posn)
;; where body is the list of Posn to represents all body's position
;; direction is current direction of a snake
;; apple is random target that snake need to reach
;; Examples:
(define INITIAL-SNAKE (make-snake (list (make-posn 1 1)
                                        (make-posn 1 2)
                                        (make-posn 1 3))
                                  "right"
                                  (make-posn (random 20)
                                             (random 20))))
(define SNAKE-0 (make-snake (list) "up" (make-posn 1 1)))
(define SNAKE-1 (make-snake (list (make-posn 1 1)
                                  (make-posn 1 2)
                                  (make-posn 5 5)
                                  (make-posn 20 20))
                            "down" (make-posn 2 2)))


#;(define (main/game g)
    (big-bang g
      [to-draw draw-body]
      [on-tick move-body faster]
      [on-key change-direction]
      [stop-when collide? draw-grade]))

;; draw-body : Snake -> Image
;; draw snake's body

(define (draw-body s)
  (local [;; draw-all : Posn Image -> Image
          ;; draw single body of the snake and put them together
          (define (draw-all posn i)
            (place-image SINGLE-BODY
                         (* 10 (posn-x posn)) (* 10 (posn-y posn)) i))]
    (foldr draw-all BG (snake-body s))))

;; move-body : Snake -> Snake
;; move snake's body and enlarge body if it reaches apple
(define (move-body s)
  (cond
    [(string=? (snake-direction s) "up") (make-snake (]
[]
[]
[]))


  
;; change-direction : Snake KeyEvent-> Snake
;; change direction of the snake
(define (change-direction s ke)
  (cond
    [(key=? ke "up") (make-snake (snake-body s)
                                 "up"
                                 (snake-apple s))]
    [(key=? ke "down") (make-snake (snake-body s)
                                   "down"
                                   (snake-apple s))]
    [(key=? ke "left") (make-snake (snake-body s)
                                   "left"
                                   (snake-apple s))]
    [(key=? ke "right") (make-snake (snake-body s)
                                    "right"
                                    (snake-apple s))]
    [else s]))
  
;; collide? : Snake -> Boolean
;; determine if snake is dead
(define (collide? s)
  (local [;; any-same-body? : [List-of Posn] -> Boolean
          ;; determine if there are the same posn of the snake's body
          (define (any-same-body? lop)
            ]
(or (any-same-body? (snake-body s))
    (out-of-background? s)))

;; draw-grade : Snake -> Image
;; draw final grade of this game
(define (draw-grade s)
  (above Final
         (text (number->string (- (length (snake-body s))
                                  3) TEXT-SIZE "red")
         BG))




