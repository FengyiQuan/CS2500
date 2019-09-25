;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample05) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; Exercise 1

;; A Minute is a (make-minute [0, 11] [0, 59] Boolean)
(define-struct minute [hr min am?])
;; and the hour of the day, minute past the hour, and whether or not it is am or pm

(define MIDNIGHT (make-minute 0 0 #t))
(define NOON (make-minute 0 0 #f))
(define ELEVEN-FIFTY-NINE-PM (make-minute 11 59 #f))

;; minute-temp : Minute -> ?
(define (minute-temp min)
  (... (minute-hr min) (minute-min min) (minute-am? min)))

;; Exercise 2

;; next-min: Minute -> Minute
;; The next minute
(check-expect (next-min MIDNIGHT) (make-minute 0 1 #t))
(check-expect (next-min ELEVEN-FIFTY-NINE-PM) MIDNIGHT)
(define (next-min min)
  (make-minute (next-hour (minute-hr min) (minute-min min))
               (next-minute (minute-min min))
               (am/pm (minute-hr min) (minute-min min) (minute-am? min))))

;; next-hour : [0, 11] [0, 59] -> [0, 11]
;; Advance to the next hour if minute is 59
(check-expect (next-hour 11 59) 0)
(check-expect (next-hour 0 0) 0)
(define (next-hour hr min)
  (if (= min 59) (modulo (add1 hr) 12) hr))

;; next-minute : [0, 59] -> [0, 59]
;; The next minute
(check-expect (next-minute 59) 0)
(check-expect (next-minute 0) 1)
(define (next-minute minute)
  (modulo (add1 minute) 60))

;; am/pm : [0, 11] [0, 59] Boolean -> Boolean
;; Flip the boolean if we are at 11:59
(check-expect (am/pm 11 59 #t) #f)
(check-expect (am/pm 11 58 #t) #t)
(check-expect (am/pm 10 59 #t) #t)
(check-expect (am/pm 10 58 #t) #t)
(define (am/pm hr min b)
  (if (and (= hr 11) (= min 59)) (not b) b))

(define FONT-SIZE 20)
(define FONT-COLOR "black")
(define BG (empty-scene 200 200))

;; Exercise 3

;; draw-minute : Minute -> Image
;; Draw the current time on a digital clock
(check-expect (draw-minute MIDNIGHT) (draw-minute MIDNIGHT))
(check-expect (draw-minute ELEVEN-FIFTY-NINE-PM) (draw-minute ELEVEN-FIFTY-NINE-PM))
(define (draw-minute min)
  (overlay
   (text (string-append (min->hour min)
                        ":"
                        (pad-zero (minute-min min))
                        " "
                        (if (minute-am? min) "am" "pm"))
         FONT-SIZE
         FONT-COLOR)
   BG))

;; min->hour : Minute -> String
;; Output the hour to display
(check-expect (min->hour MIDNIGHT) "12")
(check-expect (min->hour ELEVEN-FIFTY-NINE-PM) "11")
(define (min->hour min)
  (if (zero? (minute-hr min)) "12" (number->string (minute-hr min))))

;; pad-zero : Number -> String
;; Pad a zero if the input is less than 10
(check-expect (pad-zero 0) "00")
(check-expect (pad-zero 12) "12")
(define (pad-zero n)
  (if (< n 10) (string-append "0" (number->string n)) (number->string n)))

;; main/clock: Minute -> Number
;; Run a digital clock, produce the # of minutes that have passed since 12:00am
(define (main/clock min)
  (minutes-passed (big-bang min
                    [on-tick next-min]
                    [to-draw draw-minute])))

;; minutes-passed : Minute -> Number
;; How many minutes have passed since 12:00am
(check-expect (minutes-passed MIDNIGHT) 0)
(check-expect (minutes-passed ELEVEN-FIFTY-NINE-PM) 1439)
(define (minutes-passed min)
  (+ (minute-min min)
     (* 60 (+ (if (minute-am? min) 0 12)
              (minute-hr min)))))