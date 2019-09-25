;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |midpoint game designer|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A MidpointGame is one of:
; - (make-pre-click Posn Posn)
; - (make-post-click Posn Posn Posn)
(define-struct pre-click [p1 p2])
(define-struct post-click [p1 p2 p-click])
; and represents the two points to be clicked between
; as well as the point where the player clicked (once they do)
;; Exampels:
(define POSN-0 (make-posn 0 0))
(define POSN-10 (make-posn 10 10))
(define POSN-CLICK (make-posn 6 6))
;; Exampels:
(define MPG-PRE (make-pre-click POSN-0 POSN-10))
(define MPG-POST (make-post-click POSN-0 POSN-10 POSN-CLICK))
#;
; mg-temp : MidpointGame -> ?
(define (mg-temp mg)
  (cond [(pre-click? mg) (... (posn-temp (pre-click-p1 mg))
                              (posn-temp (pre-click-p2 mg)))]
        [(post-click? mg) (... (posn-temp (post-click-p1 mg))
                               (posn-temp (post-click-p2 mg))
                               (posn-temp (post-click-pclick mg)))]))
#;
; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

;; Exercise 4
;; handle-mouse : MidpointGame Number Number MouseEvent -> MidpointGame
;; Move to post game when someone clicks
(check-expect (handle-mouse MPG-PRE 6 6 "button-down") (click MPG-PRE 6 6))
(check-expect (handle-mouse MPG-PRE 6 6 "button-up") MPG-PRE)
(define (handle-mouse mpg x y mouse)
  (if (mouse=? mouse "button-down")
      (click mpg x y)
      mpg))

;; click : MidpointGame Number Number -> MidpointGame
;; Move to post game if you are in the pregame
(check-expect (click MPG-PRE 6 6) MPG-POST)
(check-expect (click MPG-POST 6 6) MPG-POST)
(define (click mg x y)
  (cond [(pre-click? mg) (make-post-click
                          (pre-click-p1 mg)
                          (pre-click-p2 mg)
                          (make-posn x y))]
        [(post-click? mg) mg]))

;; Exercise 5
;; midpoint : Posn Posn -> Posn
;; The midpoint of two Posns
(check-expect (midpoint POSN-0 POSN-10) POSN-5)
(define (midpoint p1 p2)
  (make-posn (/ (+ (posn-x p1) (posn-x p2)) 2)
             (/ (+ (posn-y p1) (posn-y p2)) 2)))

;; Exercise 6
;; game-midpoint : MidpointGame -> Posn
;; The midpoint of the two points in the game
(check-expect (game-midpoint MPG-PRE) POSN-5)
(check-expect (game-midpoint MPG-POST) POSN-5)
(define (game-midpoint mg)
  (cond [(pre-click? mg) (midpoint (pre-click-p2 mg) (pre-click-p1 mg))]
        [(post-click? mg) (midpoint (post-click-p2 mg) (post-click-p1 mg))]))

;; Exercise 7
;; distance : Posn Posn -> Posn
;; The distance between two Posns
(check-expect (distance (make-posn 3 4) POSN-0) 5)
(define (distance p1 p2)
  (sqrt (+ (sqr (- (posn-x p1) (posn-x p2)))
           (sqr (- (posn-y p1) (posn-y p2))))))

;; Exercise 8
;; constants :
(define DOT (circle 10 "solid" "red"))
(define DOT2 (circle 5 "solid" "yellow"))
(define DOT3 (circle 3 "solid" "blue"))

(define HEIGHT 500)
(define WIDTH 500)
(define BG (empty-scene WIDTH HEIGHT))


;; Exercise 9
;; place : Image Posn Image -> Image
;; Place i1 on i2 at p
(check-expect (place DOT POSN-10 BG) (place-image DOT 10 10 BG))
(define (place i1 p i2)
  (place-image i1 (posn-x p) (posn-y p) i2))

;; Exercise 10
;; draw-line : Posn Posn Color Image -> Image
;; Draw a line of color c from p1 to p2 on i
(check-expect (draw-line POSN-0 POSN-10 "red" BG)
              (scene+line BG 0 0 10 10 "red"))
(define (draw-line p1 p2 c i)
  (scene+line i (posn-x p1) (posn-y p1) (posn-x p2) (posn-y p2) c))

;; Exercise 11
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

;; Exercise 13
;; user-error : MidpointGame -> Number
;; How far off the user was from the midpoint
(check-error (user-error MPG-PRE))
(check-within (user-error MPG-POST) (sqrt 2) .01)
(define (user-error mg)
  (cond [(pre-click? mg) (error)]
        [else (distance (game-midpoint mg) (post-click-p-click mg))]))

;; Exercise 14















