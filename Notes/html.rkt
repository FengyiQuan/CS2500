;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname b) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Data Definition for an HTML Div:

(define-struct ul [content])
(define-struct ol [content])

; A Div is one of
; - String
; - Image
; - UL
; - OL
; A UL is a (make-ul [List-of Div])
; A OL is a (make-ol [List-of Div])

(define DIV-1 "Hi!")
(define DIV-2 "Hello!")
(define DIV-3 (make-ul (list DIV-1 DIV-2)))
(define DIV-4 (make-ol (list DIV-1 DIV-2 DIV-3)))

#;
(define (div-temp d)
  (cond [(string? d) ...]
        [(image?  d) ...]
        [(ul?     d) (ul-temp d) ...]
        [(ol?     d) (ol-temp d) ...]))

#;
(define (ul-temp d)
  (lod-temp (ul-content d)))

#;
(define (ol-temp d)
  (lod-temp (ol-content d)))

#;
(define (lod-temp lod)
  (cond [(empty? lod) ...]
        [(cons?  lod) (div-temp (first lod)) ... (lod-temp (rest lod))]))

; Design a function render-div that accepts a Div and renders it like a web browser would:
; List should be indented, ULs should have bullets, OLs should be numbered, etc.

(require 2htdp/image)
(define TEXT-SIZE 12)
(define TEXT-COLOR "black")
(define BULLET (circle 2 "solid" "black"))
 
; render-div : Div -> Image
; Render this Div

(define RENDER-1 (text DIV-1 TEXT-SIZE TEXT-COLOR))
(define RENDER-2 (text DIV-2 TEXT-SIZE TEXT-COLOR))
(define RENDER-3 (above/align "left"
			      (beside/align "middle" BULLET RENDER-1)
			      (beside/align "middle" BULLET RENDER-2)))
(define RENDER-4 (above/align "left"
                              (beside/align "middle" (text "1. " TEXT-SIZE TEXT-COLOR) RENDER-1)
                              (beside/align "middle" (text "2. " TEXT-SIZE TEXT-COLOR) RENDER-2)
                              (beside/align "middle" (text "3. " TEXT-SIZE TEXT-COLOR) RENDER-3)))

(check-expect (render-div DIV-1) RENDER-1)
(check-expect (render-div DIV-2) RENDER-2)
(check-expect (render-div DIV-3) RENDER-3)
(check-expect (render-div DIV-4) RENDER-4)

(define (render-div div)
  (cond [(string? div) (text div TEXT-SIZE TEXT-COLOR)]
        [(image?  div) div]
        [(ul?     div) (render-ul div)]
        [(ol?     div) (render-ol div)]))

; render-ul : UL -> Image
; render this unordered list
(check-expect (render-ul DIV-3) RENDER-3)
(define (render-ul ul)
  (render-ul-lod (ul-content ul)))

; render-ul-lod : [List-of Div] -> Image
; render this list of divs as a UL
(define (render-ul-lod lod)
  (local [; render-element : Div -> Image
          ; render Div and add bullet to front
          (define (render-element div)
              (beside/align "middle" BULLET (render-div div)))
          (define (on-top img1 img2)
            (above/align "left" img1 img2))]
    (foldr on-top empty-image (map render-element lod))))

; render-ol : OL -> Image
; render this OL
(define (render-ol ol)
  (render-ol-lod (ol-content ol) 1))

; render-ol-lod : [List-of Div] Nat -> Image
; render this list as a OL
(define (render-ol-lod lod i)
  (cond [(empty? lod) empty-image]
        [(cons?  lod) (above/align "left"
                                   (beside/align "middle"
                                                 (text (string-append (number->string i) ". ") TEXT-SIZE TEXT-COLOR)
                                                 (render-div (first lod)))
                                   (render-ol-lod (rest lod) (add1 i)))]))
