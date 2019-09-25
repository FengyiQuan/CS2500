;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample07) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct circl [radius mode c])
(define-struct squar [side-length mode c])
(define-struct rectangl [width height mode c])
; A Shape is one of:
; - (make-circl Number Mode Color)
; - (make-squar Number Mode Color)
; - (make-triangl Number Number Mode Color)
 
; and represents either:
; - the radius in pixels, mode, and color of a circle
; - the side length in pixels, mode, and color of a square
; - the width and height in pixels, the mode, and color of a rectangle
 
; A Mode is one of:
; - "solid"
; - "outline"

;; Exercise 1

(define SOLID "solid")
(define OUTLINE "outline")

(define CIRCLE (make-circl 5 SOLID "red"))
(define SQUARE (make-squar 5 SOLID "red"))
(define RECTANGLE (make-rectangl 5 10 OUTLINE "red"))

;; shape-temp : Shape -> ?
(define (shapte-temp s)
  (cond [(circl? s) (... (circl-radius s) (mode-temp (circl-mode s)) (circl-c s))]
        [(squar? s) (... (squar-side-length s) (mode-temp (squar-mode s)) (squar-c s))]
        [(rectangl? s) (... (rectangl-width s) (rectangl-height s)
                                  (mode-temp (rectangl-mode s))
                                  (rectangl-c s))]))

;; mode-temp : Mode -> ?
(define (mode-temp m)
  (cond [(string=? SOLID m) ...]
        [(string=? OUTLINE m) ...]))

;; Exercise 2

;; circle is a function which produces an image and circl is the name of a struct

;; Exercise 3

;; draw-shape : Shape -> Image
;; Draws a shape
(check-expect (draw-shape CIRCLE) (circle 5 "solid" "red"))
(check-expect (draw-shape SQUARE) (square 5 "solid" "red"))
(check-expect (draw-shape RECTANGLE) (rectangle 5 10 "outline" "red"))
(define (draw-shape s)
  (cond [(circl? s) (circle (circl-radius s) (circl-mode s) (circl-c s))]
        [(squar? s) (square (squar-side-length s) (squar-mode s) (squar-c s))]
        [(rectangl? s) (rectangle (rectangl-width s) (rectangl-height s)
                                  (rectangl-mode s)
                                  (rectangl-c s))]))

;; Exercise 4

;; flip-mode/shape : Shape -> Shape
;; Flip the mode of a shape
(check-expect (flip-mode/shape CIRCLE) (make-circl 5 "outline" "red"))
(check-expect (flip-mode/shape SQUARE) (make-squar 5 "outline" "red"))
(check-expect (flip-mode/shape RECTANGLE) (make-rectangl 5 10 "solid" "red"))
(define (flip-mode/shape s)
  (cond [(circl? s) (make-circl (circl-radius s) (flip-mode (circl-mode s)) (circl-c s))]
        [(squar? s) (make-squar (squar-side-length s) (flip-mode (squar-mode s)) (squar-c s))]
        [(rectangl? s) (make-rectangl (rectangl-width s) (rectangl-height s)
                                      (flip-mode (rectangl-mode s))
                                      (rectangl-c s))]))

;; flip-mode : Mode -> Mode
;; Flip the mode
(check-expect (flip-mode "outline") "solid")
(check-expect (flip-mode "solid") "outline")
(define (flip-mode m)
  (cond [(string=? m "outline") "solid"]
        [(string=? m "solid") "outline"]))


;; Exercise 5

(define-struct monkey [name color others])
;; A MonkeyChain is one of:
;; - "barrel"
;; - (make-monkey String String MonkeyChain)
;; and represents either:
;; - a barrel
;; - a monkey with a name, color, and chain of monkeys (or barrel) it is attached to

(define EMPTY-CHAIN "barrel")
(define CHAIN-1 (make-monkey "Greg" "purple" EMPTY-CHAIN))
(define CHAIN-2 (make-monkey "Cindy" "blue" CHAIN-1))
(define CHAIN-3 (make-monkey "Bob" "purple" CHAIN-2))

;; mc-temp : MonkeyChain -> ?
(define (mc-temp mc)
  (cond [(string? mc) ...]
        [(monkey? mc) (... (monkey-name mc)
                           (monkey-color mc)
                           (mc-temp (monkey-others mc)))]))
;; Exercise 6

;; how-many-purple : MonkeyChain -> Nat
;; Counts the number of purple monkeys in the chain
(check-expect (how-many-purple EMPTY-CHAIN) 0)
(check-expect (how-many-purple CHAIN-3) 2)
(define (how-many-purple mc)
  (cond [(string? mc) 0]
        [(monkey? mc)
         (if (string=? (monkey-color mc) "purple")
             (add1 (how-many-purple (monkey-others mc)))
             (how-many-purple (monkey-others mc)))]))

;; Exercise 7

;; monkey-present? : MonkeyChain String -> Boolean
;; Is a monkey named s present?
(check-expect (monkey-present? EMPTY-CHAIN "") #f)
(check-expect (monkey-present? CHAIN-3 "Greg") #t)
(check-expect (monkey-present? CHAIN-3 "") #f)
(define (monkey-present? mc s)
  (cond [(string? mc) #f]
        [(monkey? mc) (or (string=? (monkey-name mc) s)
                          (monkey-present? (monkey-others mc) s))]))

;; Exercise 8

(define FONT-SIZE 20)
(define MONKEY-NAMES-ARE (text "The monkey names are:" FONT-SIZE "violet"))

;; draw-monkey-names : MonkeyChain -> Image
;; Draw the colored names of monkeys above each other, with "The monkey names are:" above them
(check-expect (draw-monkey-names EMPTY-CHAIN) MONKEY-NAMES-ARE)
(check-expect (draw-monkey-names CHAIN-3)
              (above MONKEY-NAMES-ARE
                     (text "Bob" FONT-SIZE "purple")
                     (text "Cindy" FONT-SIZE "blue")
                     (text "Greg" FONT-SIZE "purple")))
(define (draw-monkey-names mc)
  (above MONKEY-NAMES-ARE (stack-monkey-names mc)))

;; stack-monkey-names : MonkeyChain -> Image
;; Stack monkey names on top of each other
(check-expect (stack-monkey-names EMPTY-CHAIN) empty-image)
(check-expect (stack-monkey-names CHAIN-3)
              (above (text "Bob" FONT-SIZE "purple")
                     (text "Cindy" FONT-SIZE "blue")
                     (text "Greg" FONT-SIZE "purple")))
(define (stack-monkey-names mc)
  (cond [(string? mc) empty-image]
        [(monkey? mc) (above (text (monkey-name mc) FONT-SIZE (monkey-color mc))
                             (stack-monkey-names (monkey-others mc)))]))

;; Exercise 9

;; An LoB (List of Booleans) is one of:
;; - '()
;; - (cons Boolean LoB)

(define LOB-0 '())
(define LOB-1 (cons #t LOB-0))
(define LOB-2 (cons #f LOB-1))
(define LOB-3 (cons #t LOB-2))

;; lob-temp : LoB -> ?
(define (lob-temp lob)
  (cond [(empty? lob) ...]
        [(cons? lob) (... (first lob) (lob-temp (rest lob)))]))

;; Exercise 10

;; any-true? : LoB -> Boolean
;; Is any element of this list #true?
(check-expect (any-true? LOB-0) #false)
(check-expect (any-true? LOB-2) #true)
(define (any-true? lob)
  (cond [(empty? lob) #false]
        [(cons? lob) (or (first lob) (any-true? (rest lob)))]))

;; Exercise 11

;; negate-all : LoB -> LoB
;; Negate all the booleans in lob
(check-expect (negate-all LOB-0) '())
(check-expect (negate-all LOB-3) (cons #f (cons #t (cons #f '()))))
(define (negate-all lob)
  (cond [(empty? lob) '()]
        [(cons? lob) (cons (not (first lob))
                           (negate-all (rest lob)))]))
