;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Lab 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;Exercise 4
;multiple-of-5? : num ->num
;determines if a number is a multiple of 5
(check-expect (multiple-of-5? 5) #true)
(check-expect (multiple-of-5? 7) #false)
(check-expect (multiple-of-5? 35) #true)
(check-expect (multiple-of-5? 0) #true)

(define (multiple-of-5? num)
 (= 0 (modulo num 5)))

;exercise 5
;greet : String -> string
;creates a personalized greeting for a name
(check-expect (greet "ramzi") "Waddup ramzi")
(check-expect (greet "abc") "Waddup abc")

(define GREETING "Waddup ")
(define (greet name)
  (string-append GREETING name))

;exercise 8
; times-itself : Number -> Number
; Multiply a number by itself
(check-expect (times-itself 0) 0)
(check-expect (times-itself 2) 4)

(define (times-itself n)
  (* n n))

;exercise 9
;set one more check-expect function
(check-expect (times-itself 3) 9)

;exercise 10
;percentage-of-net-worth : num -> num
(check-expect (percentage-of-net-worth 197000000000) 55/19700)
(check-expect (percentage-of-net-worth 1409000000000) 55/140900)

(define water-cost 550000000)
(define (percentage-of-net-worth someones-net-worth)
  (/ water-cost someones-net-worth))

;exercise 11
;absolute : num ->num
;computes an absolute by given a number
(check-expect (absolute 1) 1)
(check-expect (absolute 0) 0)
(check-expect (absolute -1) 1)

(define (absolute num)
  (cond
    [(>= num 0) num]
    [(< num 0) (* -1 num)]))

;exercise 12
;letter-grade ; number -> string
;consumes a Score and produces the corresponding letter grade
(check-expect (letter-grade 101) "A")
(check-expect (letter-grade 93) "A")
(check-expect (letter-grade 90) "A")
(check-expect (letter-grade 80) "B")
(check-expect (letter-grade 75) "C")
(check-expect (letter-grade 70) "C")
(check-expect (letter-grade 62) "D")
(check-expect (letter-grade 60) "D")
(check-expect (letter-grade 45) "F")
(check-expect (letter-grade -3) "F")

(define (letter-grade Num)
  (cond
   [(>= Num 90) "A"]
   [(>= Num 80) "B"]
   [(>= Num 70) "C"]
   [(>= Num 60) "D"]
   [(< Num 60) "F"]))

;exercise 13
;money-earned : number -> num
;computes the total wage by given a rate and a working hours
(check-expect (money-earned 10 40) 400)
(check-expect (money-earned 50 10) 750)

(define (money-earned hr hourly-wage)
  (cond
    [(<= hr 40) (* hr hourly-wage)]
    [(> hr 40) (* hr 1.5 hourly-wage)]))

;exercuse 14
;runtime : string -> number
;outputs runtime in minutes by given a name of movie
(check-expect (runtime "the best of youth") 366)
(check-expect (runtime "karamay") 356)
(check-expect (runtime "carlos") 339)
(check-error (run-time "gone with the wind"))

(define (runtime movie-name)
  (cond
    [(string=? movie-name "the best of youth") 366]
    [(string=? movie-name "karamay") 356]
    [(string=? movie-name "carlos") 339]))

(require 2htdp/image)
;exercise 15
;draws a house with a roof and door

(define door (rectangle 45 60 "solid" "brown"))
(define roof (triangle 200 "solid" "purple"))
(define wall (square 200 "solid" "silver"))
(define knob (circle 10 "solid" "yellow"))
(define knob-door (overlay/align "right" "center"
                                 knob door))
(define house
   (overlay/align "center" "bottom"
                 knob-door
                 (above roof wall)))

;exercise 16
;draw a house with two window
(define WINDOW (add-line (add-line (square 40 "outline" "black")
                         20 0 20 40 "black")
                         0 20 40 20 "black"))
(define house-with-window (place-image WINDOW 140 250
                                       (place-image WINDOW 40 250 house)))

;exercise 17
;scene : num -> image
;places a circle of given radius at the center of an empty-scene

(define area (empty-scene 50 50))
(define (scene x)
  (cond
    [(< x 20)
     (place-image (circle x "outline" "black")
                  25 25
                  area)]
    [else (error "radius must below 20")]))

;exercise 18
(require 2htdp/universe)
(animate scene)

;exercise 19
;to make the circle grow and shrink in a loop instead of grow and suddenly dissapear and grow again
;;;Use cond (or if if you’re feeling cheeky) to make the circle grow and shrink in a loop instead of grow
;and suddenly dissapear and grow again. Since computing the radius is now a somewhat complex computation,
;it makes sense to write a seperate helper function which only computes the radius that scene calls. Hint: quotient, even?.


  

                            
                            













