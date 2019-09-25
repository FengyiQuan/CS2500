;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; INTRODUCTIONS
;; 1. Your first homework is due tonight at 9pm. There is a 5% lateness penalty PER HOUR.
;;    That means if you are even ONE SECOND late you will get 5% off (and you might as well
;;    submit 59 minutes late anyway).
;; 2. You can submit your homework as many times as you like. We will always grade your most recent
;;    submission (even if it is late!) so be sure to submit early and often.
;; 3. You may have noticed that there is a STYLE GRADE for your homework. This is an automated
;;    grader that just checks your indentation and line width. There is no reason that you should
;;    ever have less than 100% on this grader since you can re-submit as many times as you need
;;    in order to fix your style before the deadline. There will be information on how to fix
;;    these things on Piazza.
;; 4. You will also be graded on other style things manually. See the style guide on the course
;;    webpage for more information: https://course.ccs.neu.edu/cs2500/style.html

(require 2htdp/image)
(require 2htdp/universe)

;; A Car is one of:
;; - "coupe"
;; - "hatchback"
;; - "sedan"
;; - "minivan"

(define CAR-COUPE "coupe")
(define CAR-HATCHBACK "hatchback")
(define CAR-SEDAN "sedan")
(define CAR-MINIVAN "minivan")

;; car-template : Car -> ???
#;(define (car-template car-type)
  (cond [(string=? car-type CAR-COUPE) ...]
        [(string=? car-type CAR-HATCHBACK) ...]
        [(string=? car-type CAR-SEDAN) ...]
        [(string=? car-type CAR-MINIVAN) ...]))

;; Notice the #; at the beginning. That comments out everything between a set of parentheses. This
;; allows us to copy and paste our template whenever we are building a function that works on cars
;; without having to remove a bunch of comment lines (although, as I mentioned in the last lecture
;; notes, this is unneccessary, since BSL actually does know how templates work).

;; num-doors : Car -> PositiveInteger
;; Returns the number of doors for the given car type
(check-expect (num-doors CAR-COUPE) 2)
(check-expect (num-doors CAR-HATCHBACK) 5)
(check-expect (num-doors CAR-SEDAN) 4)
(check-expect (num-doors CAR-MINIVAN) 5)
(define (num-doors car-type)
  (cond [(string=? car-type CAR-COUPE) 2]
        [(string=? car-type CAR-HATCHBACK) 5]
        [(string=? car-type CAR-SEDAN) 4]
        [(string=? car-type CAR-MINIVAN) 5]))

;; Let's talk about larger programs. In a WORLD PROGRAM we need to determine the STATE OF THE WORLD
;; in order to design the program. The state of the world is simply a way of keeping track of the
;; information that CHANGES as the program continues. For example, in our eclipse program the state
;; of the world was a Number (the x coordinate of the moon). In our sunset program the state of the
;; world was also a Number (the y coordinate of the sun). It turns out animate actually only works
;; with programs that have a Number as their world state, but we will now start designing programs
;; that have other world states.

;; Imagine a traffic light program: it draws three circles and lights up the current light. So,
;; first the green light, then the yellow light, then the red light, then the green light again,
;; and so on. For this program the state of our world will be the current COLOR of the traffic
;; light. This is best represented by a String.

;; If we're going to use other world states we need to use something more complicated than the
;; animate function. We are going to use a function called big-bang. I have done my best to
;; describe the syntax below.

#;(define (my-animation some-input-here)
  (big-bang initial-state-of-the-world
    [to-draw draw-the-state-of-the-world] ;; MANDATORY
    [on-tick change-over-time]
    [on-key change-when-keyboard-is-used]
    [on-mouse change-when-mouse-is-used]))

;; All the clauses of big-bang EXCEPT to-draw are optional. You must tell it how to draw the
;; world, but you get to decide when the world changes. If it changes over time you will need
;; an on-tick clause. If it changes when you press a key on your keyboard you will need an
;; on-key clause. If it changes when you use your mouse (e.g. when you click or drag) you will
;; need an on-mouse clause.

;; Here are the signatures and purpose statements for the functions. Please note that in your
;; program you will need to name these something more specific to your program, and you will
;; need to replace "WorldState" with whatever the state of your world is (e.g. Number in the
;; eclipse program or String in the traffic light program).

;; draw-the-state-of-the-world : WorldState -> Image
;; Draws the current state of the world

;; change-over-time : WorldState -> WorldState
;; Produce the next state of the world after the clock ticks

;; change-when-keyboard-is-used : WorldState KeyEvent -> WorldState
;; Produce the next state of the world after the user presses the given key on the keyboard

;; For more information on what a KeyEvent is, see the documentation here:
;; http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=keyevent#%28tech._world._keyevent%29

;; change-when-mouse-is-used : WorldState Number Number MouseEvent -> WorldState
;; Produce the next state of the world after using the mouse in the given location
;; (the two numbers are the x and y coordinate of the mouse on the screen)

;; For more information on what a MouseEvent is, see the documentation here:
;; http://docs.racket-lang.org/teachpack/2htdpuniverse.html?q=keyevent#%28tech._world._mouseevent%29

;; Let's work on designing that traffic light program we talked about.

;; A TrafficLight is one of:
;; - "green"
;; - "yellow"
;; - "red"
;; which represents the color of a traffic light that is lit up

(define TRAFFIC-GREEN "green")
(define TRAFFIC-YELLOW "yellow")
(define TRAFFIC-RED "red")

;; traffic-light-template : TrafficLight -> ???
(define (traffic-light-template tl)
  (cond [(string=? tl TRAFFIC-GREEN) ...]
        [(string=? tl TRAFFIC-YELLOW) ...]
        [(string=? tl TRAFFIC-RED) ...]))

;; traffic-light-program : TrafficLight -> TrafficLight
;; World program: the world is a TrafficLight
(define (traffic-light-program initial-traffic-light)
  (big-bang initial-traffic-light
    [to-draw draw-traffic-light]
    [on-tick next-traffic-light 1])) ;; The 1 means that the clock ticks every 1 second

;; Normally you should put all your constants at the top of your program after your data
;; definitions. But for simplicity I have put them here since we will use them in our
;; drawing function and I wanted you to not have to scroll to figure out where they are.

(define CIRCLE-SIZE 50)
(define CIRCLE-GREEN (circle CIRCLE-SIZE "solid" TRAFFIC-GREEN))
(define CIRCLE-YELLOW (circle CIRCLE-SIZE "solid" TRAFFIC-YELLOW))
(define CIRCLE-RED (circle CIRCLE-SIZE "solid" TRAFFIC-RED))
(define CIRCLE-BLACK (circle CIRCLE-SIZE "solid" "black"))

;; draw-traffic-light : TrafficLight -> Image
;; Draw the trafficlight with the given color lit up
(define (draw-traffic-light tl)
  (cond [(string=? tl TRAFFIC-GREEN)
         (above CIRCLE-BLACK CIRCLE-BLACK CIRCLE-GREEN)]
        [(string=? tl TRAFFIC-YELLOW)
         (above CIRCLE-BLACK CIRCLE-YELLOW CIRCLE-BLACK)]
        [(string=? tl TRAFFIC-RED)
         (above CIRCLE-RED CIRCLE-BLACK CIRCLE-BLACK)]))

;; next-traffic-light : TrafficLight -> TrafficLight
;; Produces the next color to light up in the traffic light
(define (next-traffic-light tl)
  (cond [(string=? tl TRAFFIC-GREEN) TRAFFIC-YELLOW]
        [(string=? tl TRAFFIC-YELLOW) TRAFFIC-RED]
        [(string=? tl TRAFFIC-RED) TRAFFIC-GREEN]))