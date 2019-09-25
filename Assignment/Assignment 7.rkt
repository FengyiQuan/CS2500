;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 7|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])
 
; A Shape is one of:
; - (make-circl Number Mode String)
; - (make-squar Number Mode String)
; - (make-rectangl Number Number Mode String)
; 
; Interpretation: One of multiple shapes
; - make-circl
;   - radius is the radius in pixels
;   - mode is the drawing mode
;   - c is the color to draw the circle
; - make-squar
;   - side-length is the side length in pixels
;   - mode is the drawing mode
;   - c is the color to draw the square
; - make-rectangl
;   - width is the width in pixels
;   - height is the height in pixels
;   - mode is the drawing mode
;   - c is the color to draw the square
 
; A Mode is one of:
; - "solid"
; - "outline"
; Interpretation: The drawing mode for a shape (either filled or outline)

;; Exercise 1
(define CIRCL-1 (make-circl 5 "solid" "red"))
(define CIRCL-2 (make-circl 3 "outline" "blue"))
(define SQUAR-1 (make-squar 4 "solid" "light blue"))
(define SQUAR-2 (make-squar 7 "outline" "black"))
(define RECTANGL-1 (make-rectangl 3 4 "solid" "yellow"))
(define RECTANGL-2 (make-rectangl 5 7 "outline" "green"))
#;
(define (shape-temp sp)
  (cond [(circl? sp) (... (circl-radius sp) ... (circl-mode sp) ... (circl-c sp) ...)]
        [(squar? sp) (... (squar-side-length sp) ... (squar-mode sp) ... (circle-c sp) ...)]
        [(rectangl? sp) (... (rectangl-width sp) ...
                             ... (rectangl-height sp) ...
                             ... (rectangle-mode sp) ...
                             ... (rectangl-c sp) ...)]))

;; Exercise 2
;; circle is a build-in function to draw a circle which takes 3 argument, radius, mode and color
;; circl is a structure which has 3 argument which are radius, mode and color

;; Exercise 3
;; draw-shape : Shape -> Image
;; accepts a Shape and returns an Image of that shape.
(check-expect (draw-shape CIRCL-1) (circle 5 "solid" "red"))
(check-expect (draw-shape CIRCL-2) (circle 3 "outline" "blue"))
(check-expect (draw-shape SQUAR-1) (square  4 "solid" "light blue"))
(check-expect (draw-shape SQUAR-2) (square 7 "outline" "black"))
(check-expect (draw-shape RECTANGL-1) (rectangle 3 4 "solid" "yellow"))
(check-expect (draw-shape RECTANGL-2) (rectangle 5 7 "outline" "green"))
(define (draw-shape sp)
  (cond [(circl? sp) (circle (circl-radius sp) (circl-mode sp) (circl-c sp))]
        [(squar? sp) (square (squar-side-length sp) (squar-mode sp) (squar-c sp))]
        [(rectangl? sp)
         (rectangle (rectangl-width sp) (rectangl-height sp) (rectangl-mode sp) (rectangl-c sp))]))

;; Exercise 4
;; flip-mode : Shape -> Shape
;; flips the Mode of a Shape
(check-expect (flip-mode CIRCL-1) (make-circl 5 "outline" "red"))
(check-expect (flip-mode CIRCL-2) (make-circl 3 "solid" "blue"))
(check-expect (flip-mode SQUAR-1) (make-squar  4 "outline" "light blue"))
(check-expect (flip-mode SQUAR-2) (make-squar 7 "solid" "black"))
(check-expect (flip-mode RECTANGL-1) (make-rectangl 3 4 "outline" "yellow"))
(check-expect (flip-mode RECTANGL-2) (make-rectangl 5 7 "solid" "green"))
(define (flip-mode sp)
  (cond [(circl? sp) (if (string=? (circl-mode sp) "solid")
                         (make-circl (circl-radius sp) "outline" (circl-c sp))
                         (make-circl (circl-radius sp) "solid" (circl-c sp)))]
        [(squar? sp) (if (string=? (squar-mode sp) "solid")
                         (make-squar (squar-side-length sp) "outline" (squar-c sp))
                         (make-squar (squar-side-length sp) "solid" (squar-c sp)))]
        [(rectangl? sp) (if (string=? (rectangl-mode sp) "solid")
                            (make-rectangl (rectangl-width sp) (rectangl-height sp)
                                           "outline" (rectangl-c sp))
                            (make-rectangl (rectangl-width sp) (rectangl-height sp)
                                           "solid" (rectangl-c sp)))]))

(define-struct monkey [name c others])
; A MonkeyChain is one of:
; - "barrel"
; - (make-monkey String String MonkeyChain)
; 
; Interpretation: A collection of monkeys
; - "barrel" is an empty barrel
; - make-monkey
;   - name is the name of this monkey
;   - c is the color of this monkey
;   - chain is the other monkeys (or barrel) it is attached to

;; Exercise 5
(define MonkeyChain-0 "barrel")
(define MonkeyChain-1 (make-monkey "alice" "purple" MonkeyChain-0))
(define MonkeyChain-2 (make-monkey "bob" "green" MonkeyChain-1))
(define MonkeyChain-3 (make-monkey "carl" "purple" MonkeyChain-2))
#;
(define (monkeychain-temp mkyc)
  (cond [(string? mkyc) ...]
        [(monkey? mkyc) (... (monkey-name mkyc) ... (monkey-c mkyc) ...
                             (monkeychain-temp (monkey-others mkyc) ...))]))

;; Exercise 6
;; count-purple : MonkeyChain -> Number
;; counts how many "purple" monkeys there are in a MonkeyChain
(check-expect (count-purple MonkeyChain-0) 0)
(check-expect (count-purple MonkeyChain-1) 1)
(check-expect (count-purple MonkeyChain-2) 1)
(check-expect (count-purple MonkeyChain-3) 2)
(define (count-purple mkyc)
  (cond [(string? mkyc) 0]
        [(monkey? mkyc) (if (string=? "purple" (monkey-c mkyc))
                            (add1 (count-purple (monkey-others mkyc)))
                            (count-purple (monkey-others mkyc)))]))

;; Exercise 7
;; monkey-exist? : MonkeyChain String -> Boolean
;; determine whether or not a monkey with the given name exists in the MonkeyChain
;; by given a MonkeyChain and a String representing a name
(check-expect (monkey-exist? MonkeyChain-0 "carl") #false)
(check-expect (monkey-exist? MonkeyChain-1 "alice") #true)
(check-expect (monkey-exist? MonkeyChain-2 "carl") #false)
(check-expect (monkey-exist? MonkeyChain-3 "alice") #true)
(define (monkey-exist? mkyc name)
  (cond [(string? mkyc) #false]
        [(monkey? mkyc) (if (string=? name (monkey-name mkyc)) #true
                            (monkey-exist? (monkey-others mkyc) name))]))

;; Exercise 8
;; draw-monkey : MonkeyChain -> Image
;; accepts a MonkeyChain and produces an image of the colored names of the monkeys above each other
;; and appears "The monkey names are:" in the top of the names
(check-expect (draw-monkey MonkeyChain-0) (text "The monkey names are:" 30 "violet"))
(check-expect (draw-monkey MonkeyChain-1) (above (text "The monkey names are:" 30 "violet")
                                                 (text "alice" 30 "purple")))
(check-expect (draw-monkey MonkeyChain-2) (above (text "The monkey names are:" 30 "violet")
                                                 (text "bob" 30 "green")
                                                 (text "alice" 30 "purple")))
(check-expect (draw-monkey MonkeyChain-3) (above (text "The monkey names are:" 30 "violet")
                                                 (text "carl" 30 "purple")
                                                 (text "bob" 30 "green")
                                                 (text "alice" 30 "purple")))
(define (draw-monkey mkyc)
  (above (text "The monkey names are:" 30 "violet") (name-list mkyc)))
;; name-list : MonkeyChain -> Image
;; accepts a MonkeyChain and produces an image of the colored names of the monkeys above each other
(check-expect (name-list MonkeyChain-3)(above (text "carl" 30 "purple")
                                              (text "bob" 30 "green")
                                              (text "alice" 30 "purple")))
(define (name-list mkyc)
  (cond [(string? mkyc) empty-image]
        [(monkey? mkyc) (above (text (monkey-name mkyc) 30 (monkey-c mkyc))
                               (name-list (monkey-others mkyc)))]))

;; Exercise 9
;; Booleans(Bs) is one of:
;; - '()
;; (cons Boolean Bs)
;; Booleans is a list of Boolean
;; Examples:
(define BOOLEANS-0 '())
(define BOOLEANS-1 (cons #true '()))
(define BOOLEANS-2 (cons #false BOOLEANS-1))
(define BOOLEANS-3 (cons #false BOOLEANS-2))
#;
(define (booleans-temp bs)
  (cond [(empty? bs) ...]
        [(cons? bs) (... (first bs) ... (booleans-temp (rest bs)) ...)]))

;; Exercise 10
;; true? : Booleans -> Boolean
;; determines if any element in a list of booleans is #true.
(check-expect (true? BOOLEANS-0) #false)
(check-expect (true? BOOLEANS-1) #true)
(check-expect (true? BOOLEANS-2) #true)
(check-expect (true? BOOLEANS-3) #true)
(define (true? bs)
  (cond [(empty? bs) #false]
        [(cons? bs) (if (boolean=? #true (first bs)) #true
                        (true? (rest bs)))]))

;; Exercise 11
;; convert : Booleans -> Booleans
;; negates every element in a list of booleans
(check-expect (convert BOOLEANS-0) BOOLEANS-0)
(check-expect (convert BOOLEANS-1) (cons #false BOOLEANS-0))
(check-expect (convert BOOLEANS-2) (cons #true (cons #false BOOLEANS-0)))
(check-expect (convert BOOLEANS-3) (cons #true (cons #true (cons #false BOOLEANS-0))))
(define (convert bs)
  (cond [(empty? bs) '()]
        [(cons? bs) (if (boolean=? #true (first bs)) (cons #false (convert (rest bs)))
                    (cons #true (convert (rest bs))))]))
