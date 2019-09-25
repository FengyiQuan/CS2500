;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname click-counter) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Design a World program that counts how many times a user mouse-clicks
; until they exit

(require 2htdp/image)
(require 2htdp/universe)
(require 2htdp/batch-io)

(define FILENAME "c:\\users\\wahl\\click-count.txt")

(define-struct cg [this-run total])

; A CG (ClickGame) is a (make-cg Nat Nat)
; the current total and the absolute total ever played
(define CG-0 (make-cg 5 10))

(define TEXT-SIZE 30)
(define TEXT-COLOR "blue")
(define BG (empty-scene 500 100))

; click-counter : ? -> String
; Output how many clicks have been made until the user exits
(define (click-counter _)
  (write-file FILENAME (number->string (big-bang (read-count-from-file FILENAME)
                                         [to-draw  draw-count]
                                         [on-mouse handle-click]))))

; draw-count : CG -> Image
; Draw the number of clicks made
(check-expect (draw-count CG-0) (overlay (text "||||||||||" TEXT-SIZE TEXT-COLOR) BG))
(define (draw-count cg)
  (overlay (text (num->chars (cg-total cg) "|") TEXT-SIZE TEXT-COLOR) BG))

; num->chars : Nat String -> String
; converts number into this many copies of given string
;(check-expect ...)
(define (num->chars n c)
  (foldr string-append "" (build-list n (local [(define (f i) c)] f))))

; handle-click : CG Number Number MouseEvent -> Nat
; add1 if click
(check-expect (handle-click CG-0 0 0 "button-down") (make-cg 6 11))
(define (handle-click cg x y me)
  (if (mouse=? me "button-down")
      (make-cg (add1 (cg-this-run cg)) (add1 (cg-total cg)))
      cg))

; read-count-from-file : String -> CG
; read count from file if exists, o/w 0
(check-expect (read-count-from-file "....") (make-cg 0 0))
(define (read-count-from-file filename)
  (local [(define old-numbers (if (file-exists? filename)
				(map string->number (read-lines FILENAME))
				'()))
          (define total (foldr + 0 old-numbers))]
    (make-cg 0 total)))

; write-counts-to-file : String CG -> String
; Appends this-run info from CG to file
; given non-existing file and (make-cg 5 5), make new file with contents "5"
(define (write-counts-to-file filename cg)
  (local [(define old-numbers (if (file-exists? filename)
				(map string->number (read-lines FILENAME))
				'()))
	  (define new-numbers (cons (cg-this-run cg) old-numbers))
	  (define (connect n s) (string-append (number->string n) "\n" s))
	  (define new-numbers-string (foldr connect "" new-numbers))]
    (write-file filename new-numbers-string)))
