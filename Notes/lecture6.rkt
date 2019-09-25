;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lecture6) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; ANNOUNCEMENTS
;; 1. Lab quizzes start tomorrow. The material for the quiz will be based on what you learned in
;;    lecture. They are intended as a quick measure of your understanding and are NOT a large
;;    portion of your grade.
;; 2. Homework 2 is due tonight at 9pm.
;; 3. It would be a good idea to start looking ahead at future homeworks as well. Homeworks 3 and 4
;;    are available on the course webpage. Homework 4 is the beginning of a larger project that we
;;    will come back to again and again throughout the semester.

;; RECAP
;; big-bang makes WORLD PROGRAMS which respond to: clock ticks, typing, clicking, etc.

;; my-world-program : WorldState -> WorldState
;; Here you should give a description of what your world program is for
#;(define (my-world-program initial-world-state)
  (big-bang initial-world-state
    [to-draw mandatory-drawing-function-goes-here]
    [on-tick optional-time-change-function-goes-here]
    [on-key optional-key-change-function-goes-here]
    [on-mouse optional-mouse-change-function-goes-here]))

;; MICROSOFT WORD

(require 2htdp/image)
(require 2htdp/universe)

;; The first thing we need to do whenever you design a world program is "what is the WorldState?"
;; Since we are making a text editor we will use a String for the WorldState. There are also other
;; valid data definitions which we may touch on later (I can't predict the future). Do we need a
;; new data definition? No. A String is already a defined type of data.

;; Next we will start building up our world program's main function.

;; microsoft-word : String -> String
;; Run a simple text editor
(define (microsoft-word initial-text)
  (big-bang initial-text
    [to-draw draw-word]
    [on-key type-word]))

;; In lecture these clauses were flipped but personally I like to put the to-draw clause first
;; because it is mandatory in every program so I always know I need at LEAST that clause. It's
;; a personal preference and doesn't affect your program at all.

;; Note that we cannot test the microsoft-word function because it calls big-bang. The reason for
;; this is that we cannot predict what the user will do when the program is running, so we cannot
;; predict what the function will return.

;; Now we have functions on our WISHLIST: that is, we want to have these functions but we haven't
;; defined them yet. A good idea when you do this is to write down the signatures and purpose
;; statements for each of the functions on your wishlist (just so you don't forget to design them
;; later).

;; Again, I'm going to define constants here, mid-program which is definitely not what you should
;; do. But I thought it would be confusing for you if you saw those first since they won't be
;; explained or used until our drawing function. In your assignments please place your constants
;; at the top right under your data definitions.

(define WIDTH 800)
(define HEIGHT 50)
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define TEXT-SIZE 15)
(define TEXT-COLOR "purple")

;; draw-word : String -> Image
;; Draw the current text in the text editor
(check-expect (draw-word "") BACKGROUND)
(check-expect
 (draw-word "hello world")
 (overlay (text "hello world" TEXT-SIZE TEXT-COLOR) BACKGROUND))
(define (draw-word current-text)
  (overlay (text current-text TEXT-SIZE TEXT-COLOR) BACKGROUND))

;; type-word : String KeyEvent -> String
;; Type the given letter
(check-expect (type-word "" "a") "a")
(check-expect (type-word "a" "b") "ab")
(define (type-word current-text key)
  (string-append current-text key))

;; This isn't quite what we wanted. It works as long as you only type letters or numbers but
;; pressing the shift key actually types "shift" and pressing the backspace key doesn't
;; delete a character! Let's edit our program to support backspace.

;; microsoft-word-v2 : String -> String
;; Run a simple text editor (now with backspace!)
(define (microsoft-word-v2 initial-text)
  (big-bang initial-text
    [to-draw draw-word] ;; We don't have to change how we DRAW, just how we handle keys
    [on-key type-word-v2]))

;; type-word-v2 : String KeyEvent -> String
;; Type the given letter, or delete a letter if backspace was pressed
(check-expect (type-word-v2 "" "a") "a")
(check-expect (type-word-v2 "a" "b") "ab")
(check-expect (type-word-v2 "doggo" "\b") "dogg")
(define (type-word-v2 current-text key)
  (cond [(key=? key "\b")
         (substring current-text 0 (sub1 (string-length current-text)))]
        [else (string-append current-text key)]))

;; There are still a few problems with our editor. What happens when we press backspace with an
;; empty string? We get an error! We can fix that with another conditional. HOWEVER, this is getting
;; pretty complicated. If you ever see your functions getting this complicated, you may need to
;; design a helper function.

;; How do you know when you need a helper function? Well, each function should have only ONE TASK.
;; It's clear that our function has two tasks: if you press backspace, we have to check if the
;; string is empty and if not handle the deletion of the last thing you typed; if you press another
;; key we want to add it to the string. Therefore we need two functions. In general, if you are
;; unsure of whether or not you need a helper function you probably need a helper function.

;; microsoft-word-v3 : String -> String
;; Run a simple text editor (now with even better backspace!)
(define (microsoft-word-v3 initial-text)
  (big-bang initial-text
    [to-draw draw-word]
    [on-key type-word-v3]))

;; type-word-v3 : String KeyEvent -> String
;; Type the given letter, or delete a letter if backspace was pressed on a non-empty string
(check-expect (type-word-v3 "" "a") "a")
(check-expect (type-word-v3 "a" "b") "ab")
(check-expect (type-word-v3 "kitty" "\b") "kitt")
(check-expect (type-word-v3 "" "\b") "")
(define (type-word-v3 current-text key)
  (cond [(key=? key "\b") (backspace current-text)]
        [else (string-append current-text key)]))

;; backspace : String -> String
;; Remove the last character in the string if there are any
(check-expect (backspace "") "")
(check-expect (backspace "elephant") "elephan")
(define (backspace current-text)
  (cond [(string=? current-text "") ""]
        [else (substring current-text 0 (sub1 (string-length current-text)))]))

;; NOW FOR SOMETHING COMPLETELY DIFFERENT!

;; Let's re-design our eclipse program so that the moon moves in a less linear way. We want to be
;; able to change both the x-coordinate and the y-coordinate at the SAME TIME. But we don't have the
;; power to do this yet! We will need a new kind of data called a Posn.

;; A Posn is a representation of a position with an x and y coordinate and has the following data
;; definition:

;; A Posn is a (make-posn Number Number)
;; - where the first Number is the x-coordinate
;; - and the second Number is the y-coordinate

;; How can we get the x and y coordinates out of this Posn? Well, it turns out there are some useful
;; functions that we can use: posn-x and posn-y.

(check-expect (posn-x (make-posn 1 2)) 1)
(check-expect (posn-y (make-posn 10 7)) 7)

;; You can think of a Posn as a BOX containing multiple values. You can pull the values out of the
;; box, and you can put a new box together using the make-posn function.

;; What is the TEMPLATE for a Posn?

;; posn-template : Posn -> ???
(define (posn-template p)
  (... (posn-x p) ... (posn-y p) ...))

;; The template shows us how to PULL APART a Posn. That's important so that we can work with it.

;; Note that since BSL doesn't restrict our data types, you can use the make-posn function to
;; combine any two values (not just numbers), but the thing you make will NOT be a Posn because
;; the Posn data definition specifies that the two values MUST be numbers.

(define NOT-A-CAPITAL-P-POSN (make-posn "hello" #false))
(check-expect (posn-x NOT-A-CAPITAL-P-POSN) "hello")
(check-expect (posn-y NOT-A-CAPITAL-P-POSN) #false)

;; You should NEVER use make-posn in your signatures since it is NOT a defined type of data. But
;; you can absolutely use Posn, since it IS a defined type of data.

;; How does this help us? Well, we need our WorldState to be a single value, but we want to keep
;; track of two things! This allows us to keep track of two things in a SINGLE value which is
;; exactly what we need!

;; Let's do some practice with Posns

;; x-greater-than-y? : Posn -> Boolean
;; Is the x-coordinate greater than the y-coordinate?
(check-expect (x-greater-than-y? (make-posn 0 0)) #false)
(check-expect (x-greater-than-y? (make-posn 10 5)) #true)
(define (x-greater-than-y? p) (> (posn-x p) (posn-y p)))

;; add-10-to-x : Posn -> Posn
;; Add 10 to the x-coordinate of the given Posn
(check-expect (add-10-to-x (make-posn 1 2)) (make-posn 11 2))
(check-expect (add-10-to-x (make-posn 10 20)) (make-posn 20 20))
(define (add-10-to-x p)
  (make-posn (+ 10 (posn-x p)) (posn-y p)))