;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 5
(require 2htdp/image)
(require 2htdp/universe)
;; Exercise 1
(define-struct Minute [hours minutes])
;; a minute is (make-Minute IntegerNumber IntergerNumber)
;; where hours is an integer number in [0,23] which represents hours in a clock
;; minutes is an intergal number in [0,59] which represents minutes in a clock

;; Examples:
(define Minute-1 (make-Minute 3 50))
(define Minute-2 (make-Minute 18 59))
(define Minute-3 (make-Minute 23 59))
(define Minute-4 (make-Minute 11 59))

;; minute-temp : Minute -> ???
(define (minute-temp mins)
  (... (Minute-hours mins) ...
       ... (Minute-minutes mins) ...))

;; Exercise 2
;; next-minute : Minute -> Minute
;; to show the next minute in a day
(check-expect (next-minute Minute-1) (make-Minute 3 51))
(check-expect (next-minute Minute-2) (make-Minute 19 00))
(check-expect (next-minute Minute-3) (make-Minute 00 00))
(check-expect (next-minute Minute-4) (make-Minute 12 00))

(define (next-minute mins)
  (cond [(and (= (Minute-hours mins) 23) (= (Minute-minutes mins) 59)) (make-Minute 00 00)]
        [(= (Minute-minutes mins) 59) (make-Minute (+ 1 (Minute-hours mins)) 00)]
        [(<= 0 (Minute-minutes mins) 59)
         (make-Minute (Minute-hours mins) (+ 1 (Minute-minutes mins)))]))

;; Exercise 3
;; Constant :
(define BACKGROUND-WIDTH 600)
(define BACKGROUND-HEIGHT 200)
(define BACKGROUND (empty-scene BACKGROUND-WIDTH BACKGROUND-HEIGHT))
(define TEXT-SIZE 30)
(define TEXT-COLOR "red")

;; draw-minute : Minute -> Image
;; to draw current minutes
(check-expect (draw-minute Minute-1)
              (place-image
               (text (number->string (Minute-hours Minute-1)) TEXT-SIZE TEXT-COLOR)
               200 (/ BACKGROUND-HEIGHT 2)
               (place-image
                (text " : " TEXT-SIZE TEXT-COLOR)
                (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
                (place-image
                 (text (number->string (Minute-minutes Minute-1)) TEXT-SIZE TEXT-COLOR)
                 400 (/ BACKGROUND-HEIGHT 2)
                 (place-image (text "am" TEXT-SIZE TEXT-COLOR)
                              450 (/ BACKGROUND-HEIGHT 2)
                              BACKGROUND)))))
(check-expect (draw-minute Minute-2)
              (place-image
               (text (number->string (- (Minute-hours Minute-2) 12)) TEXT-SIZE TEXT-COLOR)
               200 (/ BACKGROUND-HEIGHT 2)
               (place-image
                (text " : " TEXT-SIZE TEXT-COLOR)
                (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
                (place-image
                 (text (number->string (Minute-minutes Minute-2)) TEXT-SIZE TEXT-COLOR)
                 400 (/ BACKGROUND-HEIGHT 2)
                 (place-image (text "pm" TEXT-SIZE TEXT-COLOR)
                              450 (/ BACKGROUND-HEIGHT 2)
                              BACKGROUND)))))

(define (draw-minute mins)
  (cond
    [(< (Minute-hours mins) 12) (draw-am mins)]
    [(= (Minute-hours mins) 12) (draw-pm mins)]
    [(> 24 (Minute-hours mins) 12) (draw-pm/-12 mins)]))

;; draw-am : Minute -> Image
;; draw current minutes in the morning
(check-expect (draw-am Minute-1)
              (place-image
               (text (number->string (Minute-hours Minute-1)) TEXT-SIZE TEXT-COLOR)
               200 (/ BACKGROUND-HEIGHT 2)
               (place-image
                (text " : " TEXT-SIZE TEXT-COLOR)
                (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
                (place-image
                 (text (number->string (Minute-minutes Minute-1)) TEXT-SIZE TEXT-COLOR)
                 400 (/ BACKGROUND-HEIGHT 2)
                 (place-image (text "am" TEXT-SIZE TEXT-COLOR)
                              450 (/ BACKGROUND-HEIGHT 2)
                              BACKGROUND)))))

(define (draw-am mins)
  (place-image
   (text (number->string (Minute-hours mins)) TEXT-SIZE TEXT-COLOR)
   200 (/ BACKGROUND-HEIGHT 2)
   (place-image
    (text " : " TEXT-SIZE TEXT-COLOR)
    (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
    (place-image
     (text (number->string (Minute-minutes mins)) TEXT-SIZE TEXT-COLOR)
     400 (/ BACKGROUND-HEIGHT 2)
     (place-image (text "am" TEXT-SIZE TEXT-COLOR)
                  450 (/ BACKGROUND-HEIGHT 2)
                  BACKGROUND)))))
;; draw-pm : Minute -> Image
;; draws current minutes at noon
(check-expect (draw-pm (make-Minute 12 00))
              (place-image
               (text (number->string (Minute-hours (make-Minute 12 00))) TEXT-SIZE TEXT-COLOR)
               200 (/ BACKGROUND-HEIGHT 2)
               (place-image
                (text " : " TEXT-SIZE TEXT-COLOR)
                (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
                (place-image
                 (text (number->string (Minute-minutes (make-Minute 12 00))) TEXT-SIZE TEXT-COLOR)
                 400 (/ BACKGROUND-HEIGHT 2)
                 (place-image (text "pm" TEXT-SIZE TEXT-COLOR)
                              450 (/ BACKGROUND-HEIGHT 2)
                              BACKGROUND)))))
(define (draw-pm mins)
  (place-image
   (text (number->string (Minute-hours mins)) TEXT-SIZE TEXT-COLOR)
   200 (/ BACKGROUND-HEIGHT 2)
   (place-image
    (text " : " TEXT-SIZE TEXT-COLOR)
    (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
    (place-image
     (text (number->string (Minute-minutes mins)) TEXT-SIZE TEXT-COLOR)
     400 (/ BACKGROUND-HEIGHT 2)
     (place-image (text "pm" TEXT-SIZE TEXT-COLOR)
                  450 (/ BACKGROUND-HEIGHT 2)
                  BACKGROUND)))))

;; draw-pm/-12 : Minute -> Image
;; draws current minutes in the afternoon
(check-expect (draw-pm/-12 Minute-2)
              (place-image
               (text (number->string (- (Minute-hours Minute-2) 12)) TEXT-SIZE TEXT-COLOR)
               200 (/ BACKGROUND-HEIGHT 2)
               (place-image
                (text " : " TEXT-SIZE TEXT-COLOR)
                (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
                (place-image
                 (text (number->string (Minute-minutes Minute-2)) TEXT-SIZE TEXT-COLOR)
                 400 (/ BACKGROUND-HEIGHT 2)
                 (place-image (text "pm" TEXT-SIZE TEXT-COLOR)
                              450 (/ BACKGROUND-HEIGHT 2)
                              BACKGROUND)))))
(define (draw-pm/-12 mins)
  (place-image
   (text (number->string (- (Minute-hours mins) 12)) TEXT-SIZE TEXT-COLOR)
   200 (/ BACKGROUND-HEIGHT 2)
   (place-image
    (text " : " TEXT-SIZE TEXT-COLOR)
    (/ BACKGROUND-WIDTH 2) (/ BACKGROUND-HEIGHT 2)
    (place-image
     (text (number->string (Minute-minutes mins)) TEXT-SIZE TEXT-COLOR)
     400 (/ BACKGROUND-HEIGHT 2)
     (place-image (text "pm" TEXT-SIZE TEXT-COLOR)
                  450 (/ BACKGROUND-HEIGHT 2)
                  BACKGROUND)))))
  
;; Exercise 4
;; main/clock : Minute -> Number
;; to draw a clock
(define (main/clock mins)
  (total-mins
   (big-bang mins
     [to-draw draw-minute]
     [on-tick next-minute 1])))

;; total-mins : Minute -> Number
;; outputs the total minutes given by a Minute
(check-expect (total-mins Minute-1) 230)
(check-expect (total-mins Minute-2) 1139)
(check-expect (total-mins Minute-3) 1439)
(check-expect (total-mins Minute-4) 719)
(define (total-mins mins)
  (+ (* 60 (Minute-hours mins)) (Minute-minutes mins)))