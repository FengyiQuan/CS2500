;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A MidpointGame is one of:
; - (make-pre-click Posn Posn)
; - (make-post-click Posn Posn Posn)
(define-struct pre-click [p1 p2])
(define-struct post-click [p1 p2 p-click])
; and represents the two points to be clicked between
; as well as the point where the player clicked (once they do)
 
; A Posn is a (make-posn Number Number)
(define POSN-0 (make-posn 0 0))
(define POSN-10 (make-posn 10 10))
(define POSN-CLICK (make-posn 6 6))
 
(define MPG-PRE (make-pre-click POSN-0 POSN-10))
(define MPG-POST (make-post-click POSN-0 POSN-10 POSN-CLICK))
 
; mg-temp : MidpointGame -> ?
(define (mg-temp mg)
  (cond [(pre-click? mg) (... (posn-temp (pre-click-p1 mg))
                              (posn-temp (pre-click-p2 mg)))]
        [(post-click? mg) (... (posn-temp (post-click-p1 mg))
                               (posn-temp (post-click-p2 mg))
                               (posn-temp (post-click-pclick mg)))]))
 
; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))




;; MidpointGame Number Number MouseEvent -> MidpointGame
;; Handles mouse clikcs
(check-expect (handle-mouse MPG-PRE 6 6 "button-down") MPG-POST)

(define (handle-mouse game x y mousevent)
  (cond [(string=? mousevent "button-down") (click game x y)]
        [else game]))

;; click: MidpointGame Number Mumber
;; move to post click if you are in the pregame
(check-expect (click MPG-PRE 6 6) MPG-POST) ; pre -> post
(check-expect (click MPG-POST 6 6) MPG-POST) ; post -> do nothin'

(define (click game x y)
  (cond [(pre-click? game) (make-post-click (pre-click-p1 game)
                                            (pre-click-p2 game)
                                            (make-posn x y))]
        [(post-click? game) game]))

;; Exercise 5
;; midpoint : Posn -> Posn
;; to computes the midpoint of two Posns
(check-expect (midpoint (make-posn 0 0) (make-posn 2 2)) (make-posn 1 1))

(define (midpoint pos1 pos2)
  (make-posn (/ (abs (- (posn-x pos1) (posn-x pos2))) 2) (/ (abs (- (posn-y pos1) (posn-y pos2))) 2)))

;; Exercise 6
;; game-midpoint : Midpoint -> Posn
;; determines the midpoint of the two static dots
(check-expect (game-midpoint (make-pre-click POSN-0 POSN-10)) (make-posn 5 5))

(define (game-midpoint mpg)
  (cond
    [(pre-click? mpg)
     (midpoint (pre-click-p1 mpg) (pre-click-p2 mpg))]
    [(post-click? mpg)
     (midpoint (post-click-p1 mpg) (post-click-p2 mpg))]))

;; Exercise 7
;; distance : Posn Posn -> Number
;; computes the distance between two Posns
;(check-expect (distance POSN-0 POSN-10) (sqrt 200))

(define (distance pos1 pos2)
  (sqrt (+ (sqr (abs (- (posn-x pos1) (posn-x pos2)))) (sqr (abs (- (posn-y pos1) (posn-y pos2)))))))

;; Exercise 8
(require 2htdp/image)
;; constant :
(define WIDTH 400)
(define HEIGHT 600)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define INITIAL-POINTS (circle 5 "solid" "red"))
(define CLICK-POINT (circle 5 "solid" "black"))
(define MIDPOINT (circle 5 "solid" "green"))

;; Exercise 9
;; draw-first : Image Posn Image -> Image

(define (draw-first image1 pos image2)
  (place-image image1 (posn-x pos) (posn-y pos) image2))

;; Exercise 10
;; draw-line : Posn Posn Image Color -> Image

(define (draw-line pos1 pos2 image1 color)
  (scene+line image1 (posn-x pos1) (posn-y pos1)
              (posn-x pos2) (posn-y pos2) color))

;; Exercise 11
;; draw-MidpointGame -> MidpointGame -> Image

(define (draw-MidpointGame mpg)
  (place-image INITIAL-POINTS
               (posn-x (pre-click-p1 mpg)) (posn-y (pre-click-p1 mpg))
               (place-image INITIAL-POINTS
                            (posn-x (pre-click-p2 mpg)) (posn-y (pre-click-p2 mpg))
                            BACKGROUND)))

;; Exercise 13
;; user-error : MidpointGame -> Number
(define (user-error mpg)
  (cond
    [(post-click? mpg) (distance (game-midpoint mpg) (post-click-p-click mpg))]
    [(pre-click? mpg) (error (draw-MidpointGame mpg))]))

;; Exercise 14
;; random-pre-click : _ -> MidpointGame
(define (random-pre-click _)
  (place-image INITIAL-POINTS
               (random 600) (random 400)
  (place-image INITIAL-POINTS
               (random 600) (random 400)
  BACKGROUND)))
;; Exercise 15
(require 2htdp/universe)
;; main :
(define (main _)
  (big-bang _
    [to-draw draw-MidpointGame]
    [on-mouse user-error]
    [stop-when (mouse-event? _)]))

