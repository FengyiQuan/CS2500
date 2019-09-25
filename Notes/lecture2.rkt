;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; TO DO:
;; 1. Get a CCIS account from my.ccs.neu.edu if you haven't already
;; 2. Once you have an account log into https://handins.ccs.neu.edu and request registration for
;;    CS2500 (fall 2018, NOT the accelerated section). You will have to wait for us to approve your
;;    registration. Please don't email us to ask when we will do this. We will do it as soon as we
;;    can. Please be patient.
;; 3. Sign up for Piazza here: https://piazza.com/northeastern/fall2018/cs2500

;; ACADEMIC INTEGRITY
;; - When you are working alone you should not discuss the solutions to the homework with other
;;    students (although you can discuss high-level ideas, you should NEVER share code).
;; - Posting code on Piazza is a violation of academic integrity. If you need to share your code
;;    in order to get help the best place to go is office hours. You can also post a private
;;    question which can only be seen by the instructors.

;; DOCUMENTATION
;; - You can go to docs.racket-lang.org to see the documentation for all functions available to
;;   you in DrRacket. Be sure to view the documentation for BSL specifically, since there are
;;   many languages in DrRacket.
;; - You can also right click a function you are using and click the "Search in Help Desk" option
;;   to see the documentation for that function

(require 2htdp/image) ;; Remember, you need this line in order to draw images

;; Draw a picture of the sun in the sky
(overlay (circle 50 "solid" "yellow")
         (rectangle 600 400 "solid" "light blue"))

;; Draw a picture of the sun in the sky, but not in the center
(place-image (circle 50 "solid" "yellow") 300 375 (rectangle 600 400 "solid" "light blue"))

;; Reading the above code could be difficult since you aren't sure what the programmer was trying
;; to do. We can make this more clear by defining constants that explain what each piece of the
;; program means.

(define SUN (circle 50 "solid" "yellow"))
(define SKY (rectangle 600 400 "solid" "light blue"))
(define SETTING-SUN (place-image SUN 300 375 SKY))

;; Note that we use ALL-CAPS-WITH-DASHES-IN-BETWEEN-WORDS for the names of our constants. This is
;; a convention that makes it clear that we are defining constants which will not change. You
;; should use this convention when you are writing your code as well. For more information on style
;; conventions see the style guide on the course webpage.

;; What if we want to draw more than one thing? Say, a sky with two suns? Well, we need to draw
;; the sky, draw the first sun, and then draw the second sun on top of all that. So what we need
;; is to place-image the SUN on an image that already has a SUN in it.

(define TWO-SUN-SKY (place-image SUN 100 100 SETTING-SUN))

;; In general, it's not good practice to have numbers "hard coded" into your program. That is, your
;; numbers should be constants, or should rely on constants you have already defined.

(define SKY-WIDTH 600)
(define SKY-HEIGHT 400)
(define SKY2 (rectangle SKY-WIDTH SKY-HEIGHT "solid" "light blue"))
(define SUN-RADIUS 50)
(define SUN2 (circle SUN-RADIUS "solid" "yellow"))
;; sun in the middle of the sky
(define SUN-MID-SKY (place-image SUN (/ SKY-WIDTH 2) (/ SKY-HEIGHT 2) SKY))
;; sun at the bottom of the sky
(define SUN-BOTTOM-SKY (place-image SUN (/ SKY-WIDTH 2) (- SKY-HEIGHT (/ SUN-RADIUS 2)) SKY))

;; What if we want to generate an image of the sun at ANY given y location? Is there such a
;; function? BSL doesn't provide such a function so we will need to define one ourselves.
;; How do we define our own functions?

(define (f x) (+ (sqr x) x)) ;; f(x)=x^2+x

(define (g x y) (/ (+ (sqr x) x) y)) ;; g(x,y)=(x^2+x)/y

;; We can also use our f function to write our g function

(define (g2 x y) (/ (f x) y)) ;; I can't name two functions the same name so I named this one g2

;; Note that, unlike with constants, the names of our functions are lowercase. However, we still
;; use dashes in between words. The same goes for the arguments to functions. This is the
;; convention you should follow in your code as well. It's important to name your function
;; and your function arguments something that indicates what that function does (unlike our f and g
;; functions whose names are useless). For more information on style conventions see the style
;; guide on the course webpage.

;; We're going to learn a few other steps that will help people reading our code: the signature
;; (which tells us what inputs we expect to our function), and the purpose statement (a statement
;; about what the function does)

;; The signature is a PROMISE. It says, if you give me the right kind of value, I PROMISE that you
;; will get the right kind of value back. Otherwise, something could blow up and give you an error.
;; Your signature should only use defined data types (right now that's just Number,
;; String, Boolean, or Image, but we will have more data types later in the course).
;; When you have multiple types you just put spaces between the types.

;; The purpose statement should NOT repeat your signature. There is no need to say "this function
;; takes in a Number and ...". That's covered by your signature. You don't need to say it again.

;; square-area : Number -> Number
;; Computes the area of a square given the length of its side
(define (square-area side) (sqr side))

;; Let's go back to the sun in the sky. We want to write a function which takes in the y location
;; of the sun and produces an image of the sun in the sky.

;; place-sun : Number -> Image
;; Draw a picture of the sun in the sky at the given y location
(define (place-sun y-height)
  (place-image SUN (/ SKY-WIDTH 2) y-height SKY))

;; What if we want to animate the sun setting? In order to do this we need the universe library
(require 2htdp/universe)

;; Now we have access to a function called "animate" which will animate our function
(animate place-sun)