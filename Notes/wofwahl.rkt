;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wof-wahl) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define-struct vis [c])
(define-struct inv [c])

; A WoF-Char is one of
; - (make-vis 1String)
; - (make-inv 1String)
; Interpretation: either a visible or an invisible character of a WOF word
(define WOF-C-0 (make-vis "h"))
(define WOF-C-1 (make-inv "e"))
; Template:
#;
(define (wofc-temp wofc)
  (cond [(vis? wofc) (vis-c wofc)]
        [(inv? wofc) (inv-c wofc)]))

; A WoF is a [List-of WoF-Char]
; The state of a Wheel of Fortune Game
(define WOF-0 (list (make-inv "h")
                    (make-inv "e")
                    (make-inv "l")
                    (make-inv "l")
                    (make-inv "o")))

(define WOF-1 (list (make-inv "h")
                    (make-vis "e")
                    (make-inv "l")
                    (make-inv "l")
                    (make-inv "o")))

(define WOF-2 (list (make-inv "h")
                    (make-vis "e")
                    (make-vis "l")
                    (make-vis "l")
                    (make-inv "o")))

(define WOF-3 (list (make-vis "h")
                    (make-vis "e")
                    (make-vis "l")
                    (make-vis "l")
                    (make-inv "o")))

(define WOF-4 (list (make-vis "h")
                    (make-vis "e")
                    (make-vis "l")
                    (make-vis "l")
                    (make-vis "o")))

; wof : String -> WoF
; run a WOF game on given word
(define (wof wofw)
  (big-bang (map make-inv (split wofw))
    [to-draw   draw-wof]
    [on-key    handle-key]
    [stop-when all-vis? draw-wof]))

; split : String -> [List-of 1String]
; splits a string into a list of 1-char strings
(check-expect (split "hello") (list "h" "e" "l" "l" "o"))
(define (split s)
  (cond [(string=? s "") '()]
        [else            (cons (substring s 0 1) (split (substring s 1)))]))

; all-vis? : WoF -> Boolean
; returns #true if and only if all WoF-Chars in wofw are visible
(check-expect (all-vis? WOF-0) #false)
(check-expect (all-vis? WOF-4) #true)
(define (all-vis? wofw)
  (andmap vis? wofw))

(define TEXT-SIZE  50)
(define TEXT-COLOR "red")

; draw-wof : WoF -> Image
; draws a wof
(check-expect (draw-wof WOF-3) (beside (text "h" TEXT-SIZE TEXT-COLOR)
                                       (text "e" TEXT-SIZE TEXT-COLOR)
                                       (text "l" TEXT-SIZE TEXT-COLOR)
                                       (text "l" TEXT-SIZE TEXT-COLOR)
                                       (text "?" TEXT-SIZE TEXT-COLOR)))
(define (draw-wof wofw)
  (foldr add-to-image empty-image wofw))

; add-to-image : WoF-Char Image -> Image
; places the given character to the left of the given image
(check-expect (add-to-image (make-vis "a") empty-image) (text "a" TEXT-SIZE TEXT-COLOR))
(define (add-to-image wofc image)
  (cond [(vis? wofc) (beside (text (vis-c wofc) TEXT-SIZE TEXT-COLOR) image)]
        [(inv? wofc) (beside (text "?"          TEXT-SIZE TEXT-COLOR) image)]))

; handle-key : WoF KeyEvent -> WoF
; update the visibility of the characters based on the received key event
(check-expect (handle-key WOF-0 "e") WOF-1)
(define (handle-key wofw ke)
  (local [; new-char : WoF-Char -> WoF-Char
          ; change the given wof-char based on given key event
          (define (new-char wofc)
	    (if (and (inv? wofc) (string=? ke (inv-c wofc)))
		(make-vis ke)
	        wofc))]
    (map new-char wofw)))
