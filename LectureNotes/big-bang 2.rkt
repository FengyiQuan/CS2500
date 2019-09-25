;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |big-bang 2|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;fundies-word : String -> String
;simple texgt editor

(define (fundies-word initial-word)
  (big-bang initial-word
    [to-draw draw-word]
    [on-key word-backspace-handler]))

(define FONT-SIZE 30)
(define FONT-COLOR "beige")
(define BACKGROUND (square 200 "solid" "purple"))

;draw-word : string -> image
;visualize the current text
(check-expect (draw-word "abc")
              (overlay
               (text "abc"  FONT-SIZE FONT-COLOR)
               BACKGROUND))

(define (draw-word txt)
  (overlay
   (text txt  FONT-SIZE FONT-COLOR)
   BACKGROUND))

;word-bakspace-handler :string -> String
; processes a backspace on the input text


(check-expect (word-backspace-handler "" "/b") "")
(check-expect (word-backspace-handler "a" "/b") "")

(define (word-backspace-handler text key-event)
  (cond
    [(< (string-length text) 1) ""]
    [else (substring text 0 (sub1 (string-length text)))]))

; word-key-handler : string KeyEvent -> String
; adds a key to the text
(check-expect (word-key-handler "abc" "f") "abcf")
(check-expect (word-key-handler "" "Q") "Q")

(define (word-key-handler txt key)
  (cond
    [(string=? key "/b") (substring txt 1 (sub1 (string-length txt)))]
    [else (string-append txt key)]))