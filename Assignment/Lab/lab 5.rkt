;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |lab 5|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
; Exercise 1
; designa  data definition for lists of numbers

; a list of numbers (LON) is one of:
; - '()
; - (cons Number LON)

; Examples
(define LON1 '())
(define LON2 (cons 4 LON1))
(define LON3 (cons -7 LON2))

; Template
; LON -> ??
(define (lon-temp lon)
  (cond [(empty? lon) ...]
        [(cons? lon) (... (first lon)
                          (lon-temp (rest lon))...)]))

; Exercise 2
; designa  function any-negatives?

; any-negatives? : LON -> Boolean
; Are there any negatives in the list?

(check-expect (any-negatives? LON1) #false)
(check-expect (any-negatives? LON2) #false)
(check-expect (any-negatives? LON3) #true)

(define (any-negatives? lon)
  (cond [(empty? lon) #false]
        [(cons? lon) (or (< (first lon) 0)
                          (any-negatives? (rest lon)))]))

; Exercise 3
; design a function mean which computes the mean of a list of numbers

(check-expect (mean LON2) 4)
(check-expect (mean LON3) -1.5)

(define (mean lon)
        (/ (sum lon) (length lon)))
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon) (sum (rest lon)))]))

; Exercise 4
; design a function which only keeps the even numbers in a list of numbers

(check-expect (onlyevens LON1) '())
(check-expect (onlyevens LON2) LON2)
(check-expect (onlyevens LON3) LON2)

(define (onlyevens lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (if (even? (first lon)) (cons (first lon) (onlyevens (rest lon)))
                          (onlyevens (rest lon)))]))


;exercise 5
; design a function which adds 42 to every element in a list of numbers

; add42 : LON -> LON
; adds 42 to all elements in a list of numbers

(check-expect (add42 LON1) '())
(check-expect (add42 LON2) (cons 46 LON1))
(check-expect (add42 LON3) (cons 35 (cons 46 LON1)))

(define (add42 lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (if (number? (first lon)) (cons (+ 42 (first lon))
                          (add42 (rest lon))) '())]))

; Exercise 6
; design a function intersection which takes in two lon
; and returns the list of elements appearing in both

; intersection :  LON LON -> LON
; returns the list of elements appearing in two lists of numbers

(check-expect (intersection LON1 LON2) '())
(check-expect (intersection LON2 LON3) (cons 4 LON1))

(define (intersection lon lon2)
  (cond [(or (empty? lon) (empty? lon2)) '()]
        [(member? (first lon) lon2) (cons (first lon)
                          (intersection (rest lon) (rest lon2)))]))


; A Slideshow is one of:
; - '()
; - (cons Slide Slideshow)
; and represents an ordered slideshow
 
; A Slide is a (make-slide String LoS LoS)
(define-struct slide [title shown hidden])
; and represents a slide's title, what bullets have been shown, and which are hidden
 
; An LoS is one of:
; - '()
; - (cons String LoS)
 
(define SLIDE-1-LOS
  (cons "Value systems are neat."
        (cons "They help us determine the best ways to live."
              (cons "We each have one."
                    (cons "But what value system is best?" '())))))
(define SLIDE-1 (make-slide "Value Systems" '() SLIDE-1-LOS))
 
(define SLIDE-2-LOS
  (cons "Picking a best value system requires comparing them."
        (cons "Comparison requires measurement."
              (cons "Sample measurement: which value systems minimizes suffering."
                    (cons "But this presents a problem..." '())))))
(define SLIDE-2 (make-slide "The Best Value Systems" '() SLIDE-2-LOS))
 
(define SLIDE-3-LOS
  (cons "The minimization of suffering is a value choice."
        (cons "Another one would be maximizing happiness, for example."
              (cons "Any value system measurement is a value system itself."
                    (cons "Thus, the best value system cannot ever be determined." '())))))
(define SLIDE-3 (make-slide "Value Systems: Axiomatic" '() SLIDE-3-LOS))
 
(define SLIDESHOW (cons SLIDE-1 (cons SLIDE-2 (cons SLIDE-3 '()))))
 
; slideshow-temp : Slideshow -> ?
(define (slideshow-temp slideshow)
  (cond [(empty? slideshow) ...]
        [(cons? slideshow) (... (slide-temp (first slideshow))
                                (slideshow-temp (rest slideshow)))]))
 
; slide-temp : Slide -> ?
(define (slide-temp slide)
  (... (slide-title slide) (los-temp (slide-shown slide)) (los-temp (slide-hidden slide))))
 
; los-temp : LoS -> ?
(define (los-temp los)
  (cond [(empty? los) ...]
        [(cons? los) (... (first los)
                          (los-temp (rest los)))]))

; draw-slide ; slide -> image

(define BACKGROUND (empty-scene 200 300))

(define (draw-slide slide)
  (above (text (slide-title slide) 60 "red")
         (cond [(empty? (slide-shown slide)) (text "" 30 "blue")]
               [(cons? (slide-shown slide)) (text (slide-shown slide) 30 "blue")])))

(define (draw-slideshow slideshow)
  (cond [(empty? slideshow) (text "Fin." 80 "black")]
        [(cons? slideshow)  (draw-slide (first slideshow))
                                (draw-slideshow (rest slideshow)))]))