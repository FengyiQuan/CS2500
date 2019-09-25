;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab3-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; A MidpointGame is one of:
;; - (make-pre-click Posn Posn)
;; - (make-post-click Posn Posn Posn)
(define-struct pre-click [p1 p2])
(define-struct post-click [p1 p2 p-click])
;; and represents the two points to be clicked between
;; as well as where the player clicked (once they do)

;; A Posn is a (make-posn Number Number)
(define POSN-0 (make-posn 0 0))
(define POSN-5 (make-posn 5 5))
(define POSN-10 (make-posn 10 10))
(define POSN-CLICK (make-posn 6 6))

(define MPG-PRE (make-pre-click POSN-0 POSN-10))
(define MPG-POST (make-post-click POSN-0 POSN-10 POSN-CLICK))

;; mg-temp : MidpointGame -> ?
(define (mg-temp mg)
  (cond [(pre-click? mg) (... (posn-temp (pre-click-p1 mg))
                              (posn-temp (pre-click-p2 mg)))]
        [(post-click? mg) (... (posn-temp (post-click-p1 mg))
                               (posn-temp (post-click-p2 mg))
                               (posn-temp (post-click-p-click mg)))]))

;; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

;; to-post-game : MidpointGame Number Number MouseEvent -> MidpointGame
;; Move to post game when someone clicks  
(check-expect (to-post-game MPG-PRE 6 6 "button-down") (click MPG-PRE 6 6))
(check-expect (to-post-game MPG-PRE 6 6 "button-up") MPG-PRE)
(define (to-post-game mg x y me)
  (cond [(mouse=? me "button-down") (click mg x y)]
        [else mg]))

;; click : MidpointGame Number Number -> MidpointGame
;; Move to post game if you are in the pregame
(check-expect (click MPG-PRE 6 6) MPG-POST)
(check-expect (click MPG-POST 6 6) MPG-POST)
(define (click mg x y)
  (cond [(pre-click? mg) (make-post-click (pre-click-p1 mg)
                                          (pre-click-p2 mg)
                                          (make-posn x y))]
        [(post-click? mg) mg]))

;; midpoint : Posn Posn -> Posn
;; The midpoint of two Posns
(check-expect (midpoint POSN-0 POSN-10) POSN-5)
(define (midpoint p1 p2)
  (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
             (/ (+ (posn-y p1) (posn-y p2)) 2)))

;; game-midpoint : MidpointGame -> Posn
;; The midpoint of the two points in the game
(check-expect (game-midpoint MPG-PRE) POSN-5)
(check-expect (game-midpoint MPG-POST) POSN-5)
(define (game-midpoint mg)
  (cond [(pre-click? mg) (midpoint (pre-click-p2 mg) (pre-click-p1 mg))]
        [(post-click? mg) (midpoint (post-click-p2 mg) (post-click-p1 mg))]))

;; distance : Posn Posn -> Posn
;; The distance between two Posns
(check-expect (distance (make-posn 3 4) POSN-0) 5)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

(define DOT (circle 10 "solid" "red"))
(define DOT2 (circle 5 "solid" "yellow"))
(define DOT3 (circle 3 "solid" "blue"))

(define HEIGHT 500)
(define WIDTH 500)
(define BG (empty-scene WIDTH HEIGHT))

;; place : Image Posn Image -> Image
;; Place i1 on i2 at p
(check-expect (place DOT POSN-10 BG) (place-image DOT 10 10 BG))
(define (place i1 p i2)
  (place-image i1 (posn-x p) (posn-y p) i2))

;; draw-line : Posn Posn Color Image -> Image
;; Draw a line of color c from p1 to p2 on i
(check-expect (draw-line POSN-0 POSN-10 "red" BG)
              (scene+line BG 0 0 10 10 "red"))
(define (draw-line p1 p2 c i)
  (scene+line i (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) c))

;; draw : MidpointGame -> Image
;; Draw the game
(check-expect (draw MPG-PRE)
              (place DOT POSN-0 (place DOT POSN-10 BG)))
(check-expect (draw MPG-POST)
              (draw-line POSN-5 POSN-CLICK "red"
                         (draw-line POSN-0 POSN-10 "black"
                                    (place DOT3 POSN-5
                                           (place DOT2 POSN-CLICK
                                                  (place DOT POSN-0
                                                         (place DOT POSN-10 BG)))))))
(define (draw mg)
  (cond [(pre-click? mg) (place DOT (pre-click-p1 mg)
                                (place DOT (pre-click-p2 mg) BG))]
        [(post-click? mg)
         (draw-line (game-midpoint mg) (post-click-p-click mg) "red"
                    (draw-line (post-click-p1 mg) (post-click-p2 mg) "black"
                               (place DOT3 (game-midpoint mg)
                                      (place DOT2 (post-click-p-click mg)
                                             (place DOT (post-click-p1 mg)
                                                    (place DOT (post-click-p2 mg) BG))))))]))

;; user-error : MidpointGame -> Number
;; How far off the user was from the midpoint
(check-error (user-error MPG-PRE))
(check-within (user-error MPG-POST) (sqrt 2) .01)
(define (user-error mg)
  (cond [(pre-click? mg) (error)]
        [else (distance (game-midpoint mg) (post-click-p-click mg) )]))

;; generate-random : ? -> Posn
;; Generate a random Posn within the scene
(check-random (generate-random #f) (make-pre-click (make-posn (random WIDTH) (random HEIGHT))
                                                   (make-posn (random WIDTH) (random HEIGHT))))
(define (generate-random _)
  (make-pre-click (make-posn (random WIDTH) (random HEIGHT))
                  (make-posn (random WIDTH) (random HEIGHT))))

;; next : MidpointGame -> MidpointGame
;; A new, random pre-click world, or the same post-click world
(check-within (next MPG-PRE) (make-pre-click (make-posn 250 250) (make-posn 250 250)) 250)
(check-expect (next MPG-POST) MPG-POST)
(define (next mg)
  (cond [(pre-click? mg) (generate-random mg)]
        [else mg]))

;; next-round : MidpointGame Key -> MidpointGame
;; Launch a new game or keep it the same if not yet clicked
(check-expect (next-round MPG-PRE "a") MPG-PRE)
(check-within (next-round MPG-POST "a") (make-pre-click (make-posn 250 250) (make-posn 250 250)) 250)
(define (next-round mg k)
  (cond [(pre-click? mg) mg]
        [else (generate-random k)]))

;; main : Number -> Number
;; Play midpoint click game with new dots generate every s seconds
(define (main s)
  (user-error
   (big-bang (generate-random s)
     [on-tick next s]
     ;[stop-when post-click?]
     [on-key next-round] 
     [to-draw draw]
     [on-mouse to-post-game])))