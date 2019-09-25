;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab6-implementation-simplified) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A FIAS (Finite Increasing Arithmetic Sequence) is a:
;; (make-fias Number Number Positive)
;; where (make-fias min max step) includes all numbers
;; of the form min+k*step, where k is a natural number,
;; such that min+k*step < max.
(define-struct fias (min max step))

(define FIAS-EMPTY (make-fias 1 1 0.25)) ;; empty sequence
(define FIAS-1 (make-fias 0 1 0.25)) ;; sequence with the elements (0 .25 .5 .75)

;; increment : FIAS -> FIAS
;; Increment the fias
(check-expect (increment FIAS-1) (make-fias 0.25 1 0.25))
(define (increment fias)
  (make-fias (+ (fias-min fias) (fias-step fias)) (fias-max fias) (fias-step fias)))

;; empty-fias? : FIAS -> FIAS
;; Is this FIAS empty?
(check-expect (empty-fias? FIAS-1) #f)
(check-expect (empty-fias? FIAS-EMPTY) #t)
(define (empty-fias? fias)
  (>= (fias-min fias) (fias-max fias)))

;; sum-fias : FIAS -> Number
;; Sum all of the elements of a FIAS
(check-expect (sum-fias FIAS-EMPTY) 0)
(check-expect (sum-fias FIAS-1) 1.5)
(define (sum-fias fias)
  (fias-fold fias + 0))

;; fias-product : FIAS -> Number
;; Sum all of the elements of a FIAS
(check-expect (fias-product FIAS-EMPTY) 1)
(check-expect (fias-product FIAS-1) 0)
(define (fias-product fias)
  (fias-fold fias * 1)
  #;(cond [(empty-fias? fias) 1]
          [else (* (fias-min fias) (fias-product (increment fias)))]))

;; fias-elements : FIAS -> [List-of Number]
;; Output the elements of a FIAS
(check-expect (fias-elements FIAS-EMPTY) '())
(check-expect (fias-elements FIAS-1) (list 0 .25 .5 .75))
(define (fias-elements fias)
  (fias-fold fias cons '())
  #;(cond [(empty-fias? fias) '()]
          [else (cons (fias-min fias) (fias-elements (increment fias)))]))

;; fias-fold : FIAS [Number X -> X] X -> X
;; Fold the fias with f, base case b
(define (fias-fold fias f b)
  (cond [(empty-fias? fias) b]
        [else (f (fias-min fias) (fias-fold (increment fias) f b))]))

;; square? : Number -> Boolean
;; Is this number a perfect square?
(check-expect (square? 25) #t)
(check-expect (square? 26) #f)
(define (square? n)
  (integer? (sqrt n)))

;; any-square? : FIAS -> Boolean
;; Are any of these elements a square?
(check-expect (any-square? FIAS-EMPTY) #f)
(check-expect (any-square? FIAS-1) #t)
(define (any-square? fias)
  (any-pass? fias square?)
  #;(cond [(empty-fias? fias) #f]
          [else (or (square? (fias-min fias)) (any-square? (increment fias)))]))

;; any-even? : FIAS -> Boolean
;; Are any of these elements even?
(check-expect (any-even? FIAS-EMPTY) #f)
(check-expect (any-even? FIAS-1) #t)
(define (any-even? fias)
  (any-pass? fias even?)
  #;(cond [(empty-fias? fias) #f]
          [else (or (even? (fias-min fias)) (any-even? (increment fias)))]))

;; any-pass? : FIAS [Number -> Boolean] -> Boolean
;; Do any pass the test?
(define (any-pass? fias test)
  (cond [(empty-fias? fias) #f]
        [else (or (test (fias-min fias)) (any-pass? (increment fias) test))]))

;; the : [List-of Boolean] [Boolean -> Number] Number -> Number
(define (a supercut of us)
  (+ of
     (if (empty? supercut)
         (us #f)
         (us (first supercut)))))

;; the : X Y [X Y -> Y] [Y -> X] X Y -> Y
(define (the moments i play in the dark)
  (play (in (play the dark)) (play moments i)))

;; come : [[X -> Boolean] -> Boolean] [X -> String] [X -> Boolean] X -> String
(define (come home to my heart)
  (cond [(home my) (to heart)]
        [(my heart) " "]
        [else ""]))

;; A MidpointGame is one of:
;; - (make-pre-click [List-of Posn])
;; - (make-post-click [List-of Posn] Posn)
(define-struct pre-click [posns])
(define-struct post-click [posns p-click])
;; and represents the points to be clicked between
;; as well as where the player clicked (once they do)

;; A Posn is a (make-posn Number Number)
(define POSN-0 (make-posn 0 0))
(define POSN-5 (make-posn 5 5))
(define POSN-10 (make-posn 10 10))
(define POSN-CLICK (make-posn 6 6))

(define MPG-PRE (make-pre-click (list POSN-0 POSN-10)))
(define MPG-POST (make-post-click (list POSN-0 POSN-10) POSN-CLICK))

;; mg-temp : MidpointGame -> ?
(define (mg-temp mg)
  (cond [(pre-click? mg) (... (lop-temp (pre-click-posns mg)))]
        [(post-click? mg) (... (lop-temp (pre-click-posns mg))
                               (posn-temp (post-click-p-click mg)))]))

;; lop-temp : [List-of Posn] -> ?
(define (lop-temp lop)
  (cond [(empty? lop) ...]
        [(cons? lop) (... (posn-temp (first lop))
                          (lop-temp (rest lop)))]))

;; posn-temp : Posn -> ?
(define (posn-temp p)
  (... (posn-x p) (posn-y p)))

;; to-post-game : MidpointGame Number Number MouseEvent -> MidpointGame
;; Move to a post-game when someone clicks
(check-expect (to-post-game MPG-PRE 6 6 "button-down") MPG-POST)
(check-expect (to-post-game MPG-PRE 6 6 "button-up") MPG-PRE)
(check-expect (to-post-game MPG-POST 6 6 "button-down") MPG-POST)
(define (to-post-game mg x y me)
  (cond [(pre-click? mg)
         (if (mouse=? me "button-down")
             (make-post-click (pre-click-posns mg)
                              (make-posn x y))
             mg)]
        [(post-click? mg) mg]))

;; midpoint : [List-of Posn] -> Posn
;; The midpoint of many posns
(check-expect (midpoint (list POSN-0 POSN-10)) POSN-5)
(define (midpoint lop)
  (make-posn (/ (total lop posn-x) (length lop))
             (/ (total lop posn-y) (length lop))))

;; total : [List-of Posn] [Posn -> Number] -> Posn
;; Sum f mapped to lop
(check-expect (total (list POSN-0 POSN-10) posn-x) 10)
(define (total lop f)
  (cond [(empty? lop) 0]
        [(cons? lop) (+ (f (first lop)) (total (rest lop) f))]))

;; game-midpoint : MidpointGame -> Posn
;; The midpoint of the two points in the game
(check-expect (game-midpoint MPG-PRE) POSN-5)
(check-expect (game-midpoint MPG-POST) POSN-5)
(define (game-midpoint mg)
  (cond [(pre-click? mg) (midpoint (pre-click-posns mg))]
        [(post-click? mg) (midpoint (post-click-posns mg))]))

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


;; draw-posns : Image [List-of Posn] Image -> Image
;; Place i1 on i2 at all the spots in lop
(check-expect (draw-posns DOT (list POSN-0 POSN-10) BG)
              (place-image DOT 0 0 (place-image DOT 10 10 BG)))
(define (draw-posns i1 lop i2)
  (cond [(empty? lop) i2]
        [(cons? lop) (place i1 (first lop) (draw-posns i1 (rest lop) i2))]))

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
                         (place DOT3 POSN-5
                                (place DOT2 POSN-CLICK
                                       (place DOT POSN-0
                                              (place DOT POSN-10 BG))))))
(define (draw mg)
  (cond [(pre-click? mg) (draw-posns DOT (pre-click-posns mg) BG)]
        [(post-click? mg)
         (draw-line (game-midpoint mg) (post-click-p-click mg) "red"
                    (place DOT3 (game-midpoint mg)
                           (place DOT2 (post-click-p-click mg)
                                  (draw-posns DOT (post-click-posns mg) BG))))]))

;; user-error : MidpointGame -> Number
;; How far off the user was from the midpoint
(check-error (user-error MPG-PRE))
(check-within (user-error MPG-POST) (sqrt 2) .01)
(define (user-error mg)
  (cond [(pre-click? mg) (error)]
        [else (distance (game-midpoint mg) (post-click-p-click mg) )]))

;; generate-random-posn : ? -> Posn
;; Generate a random Posn within the scene
(check-within (generate-random-posn #f) (make-posn 250 250) 250)
(define (generate-random-posn _)
  (make-posn (random WIDTH) (random HEIGHT)))

;; generate-random-posns : Nat -> [List-of Posn]
;; Generate a random list of Posns
(check-within (generate-random-posns 2) (list (make-posn 250 250) (make-posn 250 250)) 250)
(define (generate-random-posns n)
  (cond [(zero? n) '()]
        [(positive? n) (cons (generate-random-posn #f) (generate-random-posns (sub1 n)))]))

;; next : MidpointGame -> MidpointGame
;; A new, random pre-click world, or the same post-click world
(check-within (next MPG-PRE) (make-pre-click (list (make-posn 250 250) (make-posn 250 250))) 250)
(check-expect (next MPG-POST) MPG-POST)
(define (next mg)
  (cond [(pre-click? mg) (make-pre-click (generate-random-posns (length (pre-click-posns mg))))]
        [else mg]))

;; next-round : MidpointGame Key -> MidpointGame
;; Launch a new game or keep it the same if not yet clicked
(check-expect (next-round MPG-PRE "a") MPG-PRE)
(check-within (next-round MPG-POST "a")
              (make-pre-click (list (make-posn 250 250) (make-posn 250 250)))
              250)
(define (next-round mg k)
  (cond [(pre-click? mg) mg]
        [else (make-pre-click (generate-random-posns (length (post-click-posns mg))))]))

;; main : Number -> Number
;; Play midpoint click game with new dots generate every s seconds, with n Posns
(define (main s n)
  (user-error (big-bang (make-pre-click (generate-random-posns n))
                        [on-tick next s]
                        [on-key next-round] 
                        [to-draw draw]
                        [on-mouse to-post-game])))