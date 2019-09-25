;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Recall that in our previous lecture we talked about a flight with two pilots. But what about
;; longer flights that have more than two pilots? We would have to add a field for every new
;; pilot on our plane! We did this for three pilots, and then five pilots, etc in lecture but
;; since we agreed this is not scalable I'm going to skip adding these fields and get right to
;; the next step.

;; What about passengers? How would we represent the passengers on a flight?

(define-struct passenger [name seat])
;; A Passenger is a (make-passenger String String)
;; - where name is the passenger's full name
;; - and the seat is the passenger's seat number

(define P1 (make-passenger "Scott MacKenzie" "3A"))
(define P2 (make-passenger "TJ Porter" "13C"))

;; passenger-temp : Passenger -> ???
(define (passenger-temp p)
  (... (passenger-name p) ... (passenger-seat p) ...))

(define-struct flight [passenger remainder])
;; A Flight is a (make-flight Passenger Flight)
;; - where passenger is the first passenger on the flight
;; - and remainder is the remainder of the flight

;; This seems promising! We can now put a flight INSIDE a flight and we can keep doing this forever
;; so we should be able to represent our flights of passengers no matter how many passenger are on
;; the flight. Let's try...

;; (make-flight P1 (make-flight P2 (make-flight P3 ...)))

;; The trouble is, it never ends! We can't make examples so something must be wrong with our data
;; definition. How do we fix this? We need to add a BASE CASE which allows us to stop. Something
;; that represents "there are no more passengers on the flight"

;; A Flight is one of:
;; - #false
;; - (make-flight Passenger Flight)

(define FLIGHT0 #false)
(define FLIGHT1 (make-flight P1 FLIGHT0))
(define FLIGHT2 (make-flight P2 FLIGHT1))

;; flight-template : Flight -> ???
(define (flight-template f)
  (cond [(false? f) ...]
        [(flight? f) (... (passenger-temp (flight-passenger f)) ...
                          (flight-template (flight-remainder f)) ...)]))

;; This template calls ITSELF!!! How do we know that our function that takes a flight won't run
;; on and on forever and ever? Well, every time we call the function again, we are calling it
;; with a smaller value (the value inside the flight) so we are guaranteed to eventually get to
;; the #false value and just return something. The calling of a function on itself is referred
;; to as RECURSION and it is magical and hard to understand so if you find this confusing don't
;; worry. We expect this to be confusing the first time you see it. But we are about to start
;; getting a lot of practice with this so it will (hopefully) become clear as time goes on.
;; Don't give up hope!

;; Let's consider natural numbers. If you're not sure what a natural number is, it's a non-negative
;; integer (so 0, 1, 2, etc.). How can we represent this? Well we could use a non-negative integer,
;; and that would make the most sense. But we want to show the recursive properties of this data
;; so we're going to make a different data definition. 

(define-struct nn [prev])

;; A NaturalNumber (NN) is one of:
;; - 0
;; - (make-nn NN)
;; Representing either zero or 1 more than a natural number

(define NN0 0)
(define NN1 (make-nn NN0))
(define NN2 (make-nn NN1))

;; nn-template : NN -> ???
(define (nn-template natnum)
  (cond [(number? natnum) ...]
        [(nn? natnum) (... (nn-template (nn-prev natnum)) ...)]))

;; nn->int : NN -> Integer
;; Converts an NN into integer form
(check-expect (nn->int NN0) 0)
(check-expect (nn->int NN1) 1)
(check-expect (nn->int NN2) 2)
(define (nn->int natnum)
  (cond [(number? natnum) 0]
        [(nn? natnum) (add1 (nn->int (nn-prev natnum)))]))

;; If you find this confusing a very helpful thing to do is to use the STEPPER (you can do this by
;; clicking the "Step" button in the upper right of DrRacket). This will walk you through how
;; we evaluate calls to our function. It might be helpful to copy and paste just the NaturalNumber
;; code into a new tab though, or else it will try to step through everything in this whole file.

;; is-even? : NN -> Boolean
;; Is this natural number an even number?
(check-expect (is-even? NN0) #true)
(check-expect (is-even? NN1) #false)
(define (is-even? natnum)
  (cond [(number? natnum) #true]
        [(nn? natnum) (not (is-even? (nn-prev natnum)))]))

;; Let's use our new powers of arbitrarily-sized data to create an animation of an eclipse where
;; there can be any number of moons! Each moon can have its own velocity so they can move across
;; the screen at different speeds. How would we define our data for this?

(define-struct moon [x vx])
;; A Moon is a (make-moon RealNumber RealNumber)
;; - where x is the x-position of the moon
;; - and vx is the velocity of the moon in pixels/second (from left->right)

(define MOON1 (make-moon 0 100))
(define MOON2 (make-moon 5 5))

;; moon-template : Moon -> ???
(define (moon-template m)
  (... (moon-x m) ... (moon-vx m) ...))

(define-struct system [moon remainder])
;; A System is one of:
;; - #false
;; - (make-system Moon System)
;; representing either a solar system with no moons, or a solar system with at least one moon
;;   and some other part of the solar system

(define SYSTEM0 #false)
(define SYSTEM1 (make-system MOON1 SYSTEM0))
(define SYSTEM2 (make-system MOON2 SYSTEM1))

;; system-template : System -> ???
(define (system-template sys)
  (cond [(false? sys) ...]
        [(system? sys) (... (moon-template (system-moon sys)) ...
                            (system-template (system-remainder sys)) ...)]))