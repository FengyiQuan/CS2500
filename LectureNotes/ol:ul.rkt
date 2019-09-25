;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ol:ul) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct ol [content])
(define-struct ul [content])

; A Div is one of:
; - String
; - Image
; - OL
; - UL

; A OL is a (make-ol [List-of Div])
; A UL is a (make-ul [List-of Div])
; The structure of an HTML page,
; where OL is an ordered list and UL is an unordered list
#;
(define (div-temp div)
  (cond
    [(string? div) ...]
    [(image? div) ...]
    [(ol? div) ... (ol-temp div) ...]
    [(ul? div) ... (ul-temp div) ...]))
#;
(define (ol-temp ol)
  (... (lool-temp (ol-content ol)) ...))
#;
(define (ul-temp ul)
  ... (loul-temp (ul-content ul)) ...)
#;
(define (lool-temp lool)
  (cond
    [(empty? lool) ...]
    [(cons? lool) ...
     (div-temp (first lool)) ...
     (lool-temp (rest lool)) ...]))
#;
(define (loul-temp loul)
  (cond
    [(empty? loul) ...]
    [(cons? loul) ...
     (div-temp (first loul)) ...
     (loul-temp (rest loul)) ...]))

(define DIV-1 "HI!")
(define DIV-2 "Hello!")
(define DIV-3 (make-ol (list DIV-1 DIV-2)))
(define DIV-4 (make-ul (list DIV-1 DIV-2 DIV-3)))

; num-divs : Div -> Number
;; counts the total number of Divs contained within this div

(check-expect (num-divs DIV-1) 1)
(check-expect (num-divs DIV-2) 1)
(check-expect (num-divs DIV-3) 3)
(check-expect (num-divs DIV-4) 6)

(define (num-divs div)
    (cond
    [(string? div) 1]
    [(image? div) 1]
    [(ol? div) (num-in-ol div)]
    [(ul? div) (num-in-ul div)]))

;; num-in-ol : OL -> Number
; count the divs in the OL
(check-expect (num-in-ol DIV-3) 3)
(define (num-in-ol ol)
  (add1 (num-in-lod (ol-content ol))))

;; num-in-ul : UL -> Number
; count the divs in the UL
(check-expect (num-in-ul DIV-4) 6)
(define (num-in-ul ul)
  (add1 (num-in-lod (ul-content ul))))

;; num-in-lod : [List-of Div] -> Number
;; count the divs in the list of divs
(check-expect (num-in-lod (list DIV-1 DIV-2 DIV-3)) 5)

(define (num-in-lod lod)
  (local [; add-num-divs-to-sum : Div Num -> Number
          (define (add-num-divs-to-sum d n)
            (+ n (num-divs d)))]
    (foldr add-num-divs-to-sum 0 lod)))

