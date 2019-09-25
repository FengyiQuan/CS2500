;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab7-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define WIDTH 500)
(define HEIGHT 500)
(define GRAVITY .1)
(define BALL-FREQUENCY 14)
(define BG (empty-scene WIDTH HEIGHT))
(define WINNER (overlay (text "Winner!" 20 "blue") BG))
(define LOSER (overlay (text "Loser!" 20 "red") BG))

;; A ClickGame is a (make-click-game [List-of Ball] Number Number Number)
(define-struct click-game [balls time clicked missed])
;; and represents the list of balls on screen, the time passed, the #clicked, and the #missed

;; A Ball is a (make-ball Posn Posn Number Color)
(define-struct ball [pos vel rad color])
;; and represents its position, velocity, radius, and color

;; A Posn is a (make-posn Number Number)

(define POSN-10 (make-posn 10 10))
(define POSN-2 (make-posn 2 2))

(define BALL-1 (make-ball POSN-10 POSN-2 5 "red"))
(define BALL-2 (make-ball (make-posn (add1 WIDTH) 0) (make-posn 0 0) 5 "red"))

(define GAME-1 (make-click-game (list BALL-1) 10 1 1))
(define GAME-2 (make-click-game (list BALL-2) 1 0 0))

;; clickgame-temp : ClickGame -> ?
(define (clickgame-temp cg)
  (... (lob-temp (balls-click-game cg))
       (click-game-time cg)
       (click-game-clicked cg)
       (click-game-missed cg)))

;; lob-temp : [List-of Ball] -> ?
(define (lob-temp lob)
  (... (cond [(empty? lob) ...]
             [(cons? lob) (... (ball-temp (first lob))
                               (lob-temp (rest lob)))])))

;; ball-temp : Ball -> ?
(define (ball-temp b)
  (... (posn-temp (ball-pos b)) (posn-temp (ball-vel b)) (ball-rad b) (ball-color b)))

;; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

;; modify-balls : [[List-of Ball] -> [List-of Ball]] ClickGame -> ClickGame
;; Modify the list of balls
(check-expect (modify-balls rest GAME-1) (make-click-game '() 10 1 1))
(define (modify-balls balls-modifier cg)
  (make-click-game (balls-modifier (click-game-balls cg))
                   (click-game-time cg)
                   (click-game-clicked cg)
                   (click-game-missed cg)))

;; modify-time : [Number -> Number] ClickGame -> ClickGame
;; Modify time
(check-expect (modify-time add1 GAME-1) (make-click-game (list BALL-1) 11 1 1))
(define (modify-time time-modifier cg)
  (make-click-game (click-game-balls cg)
                   (time-modifier (click-game-time cg))
                   (click-game-clicked cg)
                   (click-game-missed cg)))

;; modify-clicked : [Number -> Number] ClickGame -> ClickGame
;; Modify clicked
(check-expect (modify-clicked add1 GAME-1) (make-click-game (list BALL-1) 10 2 1))
(define (modify-clicked clicked-modifier cg)
  (make-click-game (click-game-balls cg)
                   (click-game-time cg)
                   (clicked-modifier (click-game-clicked cg))
                   (click-game-missed cg)))

;; modify-missed : [Number -> Number] ClickGame -> ClickGame
;; Modify missed
(check-expect (modify-missed add1 GAME-1) (make-click-game (list BALL-1) 10 1 2))
(define (modify-missed missed-modifier cg)
  (make-click-game (click-game-balls cg)
                   (click-game-time cg)
                   (click-game-clicked cg)
                   (missed-modifier (click-game-missed cg))))

;; main : Number Number -> Number
;; Given the limit on balls that can be missed and hit, play the game
;; and produce the time passed
(define (main missed clicked)
  (local [;; click-game-over?/main : ClickGame -> Boolean
          ;; Is the game over?
          (define (click-game-over?/main cg)
            (click-game-over? cg missed clicked))
          ;; final-screen/main : ClickGame -> Image
          ;; Final screen
          (define (final-screen/main cg)
            (final-screen cg missed clicked))]
    (click-game-time (big-bang (make-click-game '() 0 0 0)
                               [on-tick advance-time]
                               [on-mouse click]
                               [stop-when click-game-over?/main final-screen/main]
                               [to-draw draw]))))

;; advance-time : ClickGame -> ClickGame
;; Advance the time
(check-expect (advance-time GAME-1)
              (make-click-game (list (make-ball
                                      (make-posn 12 12)
                                      (make-posn 2 (+ GRAVITY 2))
                                      5
                                      "red")) 11 1 1))
(check-expect (advance-time GAME-2)
              (make-click-game '() 2 0 1))
(define (advance-time cg)
  (increment-time
   (generate-ball-if-time
    (apply-gravity
     (move-balls
      (remove-offscreen
       (increment-missed-offscreen cg)))))))


;; increment-time : ClickGame -> ClickGame
;; Increase time
(check-expect (increment-time GAME-1)
              (make-click-game (list BALL-1) 11 1 1))
(define (increment-time cg)
  (modify-time add1 cg))

;; generate-ball-if-time : ClickGame -> ClickGame
;; Generate a ball if it is the right time
(check-expect (generate-ball-if-time GAME-1) GAME-1)
(check-random (generate-ball-if-time (make-click-game '() 0 0 0))
              (make-click-game
               (list (make-ball (make-posn (+ (/ WIDTH 5) (* 3/5 (random WIDTH))) 0)
                                (make-posn (* (if (zero? (random 2)) 1 -1) (random 3)) .1)
                                (+ 10 (random 10))
                                (make-color (random 256) (random 256) (random 256))))
               0 0 0))
(define (generate-ball-if-time cg)
  (local [;; new-ball : ? -> Ball
          ;; Generate a new ball
          (define (new-ball _)
            (make-ball (make-posn (+ (/ WIDTH 5) (* 3/5 (random WIDTH))) 0)
                       (make-posn (* (if (zero? (random 2)) 1 -1) (random 3)) .1)
                       (+ 10 (random 10))
                       (make-color (random 256) (random 256) (random 256))))
          ;; add-new-ball-if-appropriate : [List-of Ball] -> [List-of Ball]
          ;; Add a new ball if it's time
          (define (add-new-ball-if-appropriate lob)
            (if (zero? (modulo (click-game-time cg) BALL-FREQUENCY))
                (cons (new-ball #f) lob)
                lob))]
    (modify-balls add-new-ball-if-appropriate cg)))

;; apply-gravity : ClickGame -> ClickGame
;; Apply gravity to cg
(check-expect (apply-gravity GAME-1)
              (make-click-game (list (make-ball POSN-10 (make-posn 2 (+ 2 GRAVITY)) 5 "red"))
                               10 1 1))
(define (apply-gravity cg)
  (local [;; apply-gravity-to-ball : Ball -> Ball
          ;; Apply gravity to a ball
          (define (apply-gravity-to-ball b)
            (make-ball (ball-pos b)
                       (make-posn (posn-x (ball-vel b))
                                  (+ GRAVITY (posn-y (ball-vel b))))
                       (ball-rad b)
                       (ball-color b)))
          ;; apply-gravity-to-balls : [List-of Ball] -> [List-of Ball]
          ;; Apply gravity-to-balls
          (define (apply-gravity-to-balls lob)
            (map apply-gravity-to-ball lob))]
    (modify-balls apply-gravity-to-balls cg)))

;; move-balls : ClickGame -> ClickGame
;; Move the balls
(check-expect (move-balls GAME-1)
              (make-click-game (list (make-ball (make-posn 12 12) POSN-2 5 "red")) 10 1 1))
(define (move-balls cg)
  (local [;; move-ball : Ball -> Ball
          ;; Apply vel to pos
          (define (move-ball b)
            (make-ball (make-posn (+ (posn-x (ball-vel b)) (posn-x (ball-pos b)))
                                  (+ (posn-y (ball-vel b)) (posn-y (ball-pos b))))
                       (ball-vel b)
                       (ball-rad b)
                       (ball-color b)))
          ;; move-all-balls : [List-of Ball] -> [List-of Ball]
          ;; Move all balls
          (define (move-all-balls lob)
            (map move-ball lob))]
    (modify-balls move-all-balls cg)))

;; remove-offscreen : ClickGame -> ClickGame
;; Remove offscreen balls
(check-expect (remove-offscreen GAME-1) GAME-1)
(check-expect (remove-offscreen GAME-2)
              (make-click-game '() 1 0 0))
(define (remove-offscreen cg)
  (local [;; is-ball-onscreen? : Ball -> Boolean
          ;; Is the ball not offscreen?
          (define (is-ball-onscreen? b)
            (not (is-ball-offscreen? b)))
          ;; remove-offscreen-balls : [List-of Ball] -> [List-of Ball]
          ;; Remove offscreen balls
          (define (remove-offscreen-balls lob)
            (filter is-ball-onscreen? lob))]
    (modify-balls remove-offscreen-balls cg)))

;; increment-missed-offscreen : ClickGame -> ClickGame
;; Increment the balls that were missed
(check-expect (increment-missed-offscreen GAME-1) GAME-1)
(check-expect (increment-missed-offscreen GAME-2)
              (make-click-game (list BALL-2) 1 0 1))
(define (increment-missed-offscreen cg)
  (local [;; increment-missed : Number -> Number
          ;; Increment the number missed
          (define (increment-missed previous-number)
            (+ previous-number (length (filter is-ball-offscreen? (click-game-balls cg)))))]
    (modify-missed increment-missed cg)))

;; is-ball-offscreen? : Ball -> Boolean
;; Is the ball offscreen?
(check-expect (is-ball-offscreen? BALL-1) #f)
(check-expect (is-ball-offscreen? BALL-2) #t)
(define (is-ball-offscreen? b)
  (or (not (<= 0 (posn-x (ball-pos b)) WIDTH))
      (not (<= 0 (posn-y (ball-pos b)) HEIGHT))))

;; click : ClickGame Number Number MouseEvent -> ClickGame
;; Increment clicked and remove clicked
(check-expect (click GAME-1 11 11 "button-down") (make-click-game '() 10 2 1))
(check-expect (click GAME-1 30 30 "button-down") GAME-1)
(check-expect (click GAME-1 30 30 "button-up") GAME-1)
(define (click cg x y me)
  (cond [(not (mouse=? me "button-down")) cg]
        [else (filter-clicked (increment-clicked cg x y) x y)]))

;; filter-clicked : ClickGame Number Number -> ClickGame
;; Filter the balls that were clicked
(check-expect (filter-clicked GAME-1 11 11) (make-click-game '() 10 1 1))
(define (filter-clicked cg x y)
  (local [;; not-clicked? : Ball -> Boolean
          ;; Was the ball not clicked?
          (define (not-clicked? b)
            (not (in? b x y)))
          ;; not-clicked-balls : [List-of Ball] -> [List-of Ball]
          ;; Balls that were not clicked
          (define (not-clicked-balls lob)
            (filter not-clicked? lob))]
    (modify-balls not-clicked-balls cg)))

;; increment-clicked : ClickGame Number Number -> ClickGame
;; Increment clicked by the balls that were clicked
(check-expect (increment-clicked GAME-1 11 11) (make-click-game (list BALL-1) 10 2 1))
(define (increment-clicked cg x y)
  (local [;; clicked? : Ball -> Boolean
          ;; Was the ball clicked?
          (define (clicked? b)
            (in? b x y))
          ;; increase-clicked : Number -> Number
          ;; Increase clicked by # clicked
          (define (increase-clicked previousy-clicked)
            (+ previousy-clicked (length (filter clicked? (click-game-balls cg)))))]
    (modify-clicked  increase-clicked cg)))

;; in? : Ball Number Number -> Boolean
;; Is this in the ball?
(check-expect (in? BALL-1 11 11) #t)
(check-expect (in? BALL-1 30 11) #f)
(define (in? b x y)
  (<= (sqrt (+ (sqr (- x (posn-x (ball-pos b)))) (sqr (- y (posn-y (ball-pos b)))))) (ball-rad b)))

;; click-game-over? : ClickGame Number Number -> Boolean
;; Is the game over?
(check-expect (click-game-over? GAME-1 2 1) #t)
(check-expect (click-game-over? GAME-1 1 2) #t)
(check-expect (click-game-over? GAME-1 2 2) #f)
(define (click-game-over? cg missed-max-allowed hit-min-needed)
  (or (missed-too-many? cg missed-max-allowed)
      (hit-enough? cg hit-min-needed)))

;; missed-too-many? : ClickGame Number -> Boolean
;; Were too many missed?
(check-expect (missed-too-many? GAME-1 2) #f)
(check-expect (missed-too-many? GAME-1 1) #t)
(define (missed-too-many? cg missed-max-allowed)
  (>= (click-game-missed cg) missed-max-allowed))

;; hit-enough? : ClickGame Number -> Boolean
;; Were too many missed?
(check-expect (hit-enough? GAME-1 2) #f)
(check-expect (hit-enough? GAME-1 1) #t)
(define (hit-enough? cg hit-min-needed)
  (>= (click-game-clicked cg) hit-min-needed))

;; final-screen : ClickGame Number Number -> Boolean
;; Show the final screen
(check-expect (final-screen GAME-1 0 2) LOSER)
(check-expect (final-screen GAME-1 2 0) WINNER)
(define (final-screen cg missed-max-allowed hit-min-needed)
  (cond [(missed-too-many? cg missed-max-allowed) LOSER]
        [(hit-enough? cg hit-min-needed) WINNER]))

;; draw : ClickGame -> Image
;; Draw the game if tile
(check-expect (draw GAME-1)
              (place-image (circle 5 "solid" "red") 10 10 BG))
(define (draw cg)
  (local [;; draw-ball : Ball Image -> Image
          ;; Draw b on i
          (define (draw-ball b i)
            (place-image (circle (ball-rad b) "solid" (ball-color b))
                         (posn-x (ball-pos b))
                         (posn-y (ball-pos b))
                         i))]
    (foldr draw-ball BG (click-game-balls cg))))