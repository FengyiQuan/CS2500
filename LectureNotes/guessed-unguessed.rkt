;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname guessed-unguessed) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct guessed [char])
(define-struct unguessed [char])

; A WoF-1String is one of:
; - (make-guessed 1String)
; - (make-unguessed 1String)

(define WOF-1STRING-1 (make-guessed "a"))
(define WOF-1STRING-2 (make-unguessed "a"))

(define (wof-1string-temp wof1)
  (cond
    [(guessed? wof1) (... (guessed-char wof1) ...)]
    [(unguessed? wof1) (... (unguessed-char wor1) ...)]))

; A Wof is a [List-of WoF-1String]
; A [List-of X] is one of:
; - '()
; - (cons X [List-of X])
;; Examples:
(define WOF-1 (list (make-unguessed "j")
                    (make-unguessed "a")
                    (make-unguessed "z")
                    (make-unguessed "z")))
(define WOF-2 (list (make-unguessed "j")
                    (make-guessed "a")
                    (make-unguessed "z")
                    (make-unguessed "z")))
(define WOF-3 (list (make-guessed "j")
                    (make-guessed "a")
                    (make-unguessed "z")
                    (make-unguessed "z")))
(define WOF-4 (list (make-guessed "j")
                    (make-guessed "a")
                    (make-guessed "z")
                    (make-guessed "z")))

#;
(define (wof-temp wof)
  (cond
    [(empty? wof) ...]
    [(cons? wof) ... (wof-1string-temp (first wof)) ...
                 (wof-temp (rest wof)) ...]))

;; main : String -> WoF
; play a game of Wheel-of-Fortune'ish

(define (main s)
  (big-bang (string-to-wof s)
    [to-draw wof-to-string]
    [on-key change-wof]
    [stop-when done-wof?]))

;; string-to-wof : String -> WoF
;; converts a string to a list of unguessed string
(check-expect (string-to-wof "jazz") WOF-1)
(define (string-to-wof s)
  (map make-unguessed (explode s)))

; done-wof? : WoF -> Boolean
; returns #true if all list elements are guessed
(check-expect (done-wof? WOF-1) #false)
(check-expect (done-wof? WOF-2) #false)
(check-expect (done-wof? WOF-3) #false)
(check-expect (done-wof? WOF-4) #true)
(define (done-wof? wof)
  (andmap guessed? wof))
; (not (ormap unguessed? wof)))  ; equivelent

;;
(define FONT-SIZE 32)
(define FONT-COLOR "peru")

;; wof-to-string : Wof -> Image
;; draws a word with ?'s for unguessed letters
(check-expect (wof-to-string WOF-1) "????")
(check-expect (wof-to-string WOF-2) "?a??")
(check-expect (wof-to-string WOF-3) "ja??")
;(check-expect (wof-to-string WOF-3) "jazz")

(define (wof-to-string wof)
  (foldr append-wof1-to-string "" wof))

;; append-wof1-to-string : WoF-1String String -> String
; either adds the letter or ? to an existing string

(check-expect (append-wof1-to-string (make-unguessed "a") ":)") "?:)")
(check-expect (append-wof1-to-string (make-guessed "a") ":)") "a:)")

(define (append-wof1-to-string wof1 s)
  (cond
    [(guessed? wof1) (string-append (guessed-char wof1) s)]
    [(unguessed? wof1) (string-append "?" s)]))
;; change-wof : WoF KeyEvent -> WoF
;; changes all the unguessed to guessed for matching letters
(check-expect (change-wof WOF-1 "a") WOF-2)
(check-expect (change-wof WOF-2 "j") WOF-3)
(check-expect (change-wof WOF-3 "z") WOF-4)

(define (change-wof wof ke)
  (local [; f : WoF-1String KeyEvent -> WoF-1Strign
          ; changes an unguessed letter to a matching letter
          ; (f (make-unguessed "a") "a") -> (make-guessed "a")
          (define (f wof1)
            (cond
              [(guessed? wof1) wof1]
              [(unguessed? wof1) (if (string=? ke (unguessed-char wof1))
                                     (make-guessed (unguessed-char wof1))
                                     wof1)]))]
    (map f wof)))



















