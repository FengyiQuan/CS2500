;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab5-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A LoN (ListOfNumbers) is one of:
;; - '()
;; - (cons Number LoN)

(define LON-0 '())
(define LON-1 (cons 1 LON-0))
(define LON-2 (cons 2 LON-1))
(define LON-3 (cons 3 LON-2))

;; lon-temp : LoN -> ?
(define (lon-temp lon)
  (cond [(empty? lon) ...]
        [(cons? lon) (... (first lon) (lon-temp (rest lon)))]))

;; any-negatives? : LoN -> Boolean
;; Is any number in this list negative?
(check-expect (any-negatives? LON-0) #f)
(check-expect (any-negatives? LON-3) #f)
(check-expect (any-negatives? (cons -1 '())) #t)
(define (any-negatives? lon)
  (cond [(empty? lon) #f]
        [(cons? lon) (or (negative? (first lon))
                         (any-negatives? (rest lon)))]))

;; mean : LoN -> Number
;; Mean of LoN
(check-error (mean LON-0))
(check-expect (mean LON-3) 2)
(define (mean lon)
  (/ (sum lon) (length lon)))


;; sum : LoN -> Number
;; The sum of the elements of the list of numbers
(check-expect (sum LON-0) 0)
(check-expect (sum LON-3) 6)
(define (sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon) (sum (rest lon)))]))


;; evens : LoN -> LoN
;; Keep only the even numbers
(check-expect (evens LON-0) LON-0)
(check-expect (evens LON-3) (cons 2 '()))
(define (evens lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (if (even? (first lon))
                         (cons (first lon) (evens (rest lon)))
                         (evens (rest lon)))]))

;; add-42 : LoN -> LoN
;; Add 42 everywhere
(check-expect (add-42 '()) '())
(check-expect (add-42 LON-3) (cons 45 (cons 44 (cons 43 '()))))
(define (add-42 lon)
  (cond [(empty? lon) '()]
        [(cons? lon) (cons (+ 42 (first lon)) (add-42 (rest lon)))]))

;; intersection : LoN LoN -> LoN
;; The numbers appearing in both
(check-expect (intersection LON-0 LON-3) '())
(check-expect (intersection (cons 3 (cons 4 '())) LON-3) (cons 3 '()))
(define (intersection lon1 lon2)
  (cond [(empty? lon1) '()]
        [(cons? lon1) (if (member (first lon1) lon2)
                          (cons (first lon1) (intersection (rest lon1) lon2))
                          (intersection (rest lon1) lon2))]))

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
  (... (cond [(empty? slideshow) ...]
             [(cons? slideshow) (... (slide-temp (first slideshow))
                                     (slideshow-temp (rest slideshow)))])))
 
; slide-temp : Slide -> ?
(define (slide-temp slide)
  (... (slide-title slide) (los-temp (slide-shown slide)) (los-temp (slide-hidden slide))))
 
; los-temp : LoS -> ?
(define (los-temp los)
  (... (cond [(empty? los) ...]
             [(cons? los) (... (first los)
                               (los-temp (rest los)))])))

(define FONT-SIZE 15)
(define FONT-COLOR "black")
(define BG (empty-scene 500 500))

;; draw-slide : Slide -> Image
;; Draw the slide
(check-expect (draw-slide SLIDE-1) (overlay (draw-bullets (cons "Value Systems" '())) BG))
(define (draw-slide slide)
  (overlay (draw-bullets (cons (slide-title slide) (slide-shown slide))) BG))

;; draw-bullets : LoS -> Image
;; Draw the bullets
(check-expect (draw-bullets '()) empty-image)
(check-expect (draw-bullets (cons "a" (cons "b" '())))
              (above (text "a" FONT-SIZE FONT-COLOR) (text "b" FONT-SIZE FONT-COLOR)))
(define (draw-bullets los)
  (cond [(empty? los) empty-image]
        [(cons? los) (above (text (first los) FONT-SIZE FONT-COLOR)
                            (draw-bullets (rest los)))]))

;; draw-slideshow : Slideshow -> Image
;; Draw the slideshow
(check-expect (draw-slideshow '()) (draw-slide (make-slide "Fin." '() '())))
(check-expect (draw-slideshow SLIDESHOW) (draw-slide SLIDE-1))
(define (draw-slideshow slideshow)
  (cond [(empty? slideshow) (draw-slide (make-slide "Fin." '() '()))]
        [(cons? slideshow) (draw-slide (first slideshow))]))

;; advance-slide : Slide -> Slide
;; Advance a slide
(check-expect (advance-slide (make-slide "" '() '())) (make-slide "" '() '()))
(check-expect (advance-slide (make-slide "" '() (cons "a" '()))) (make-slide "" (cons "a" '()) '()))
(define (advance-slide slide)
  (cond [(empty? (slide-hidden slide)) slide]
        [(cons? (slide-hidden slide))
         (make-slide (slide-title slide)
                     (append (slide-shown slide) (cons (first (slide-hidden slide)) '()))
                     (rest (slide-hidden slide)))]))

;; slide-over? : Slide -> Boolean
;; Is the slide over?
(check-expect (slide-over? SLIDE-1) #f)
(check-expect (slide-over? (make-slide "" '() '())) #t)
(define (slide-over? slide)
  (empty? (slide-hidden slide)))

;; advance-slideshow : Slideshow -> Slideshow
;; Advance the slideshow
(check-expect (advance-slideshow '()) '())
(check-expect (advance-slideshow (cons (make-slide "" '() '()) '())) '())
(check-expect (advance-slideshow (cons (make-slide "" '() (cons "a" '())) '()))
              (cons (make-slide "" (cons "a" '()) '()) '()))
(define (advance-slideshow slideshow)
  (cond [(empty? slideshow) '()]
        [(cons? slideshow) (if (slide-over? (first slideshow))
                               (rest slideshow)
                               (cons (advance-slide (first slideshow))
                                     (rest slideshow)))]))

;; slideshow : Slideshow -> Slideshow
;; Play a slideshow
(define (slideshow slideshow)
  (big-bang slideshow
    [on-tick advance-slideshow 2]
    [stop-when empty? draw-slideshow]
    [to-draw draw-slideshow]))

;; in-slideshow? : String Slideshow -> Boolean
;; Is this string anywhere in the slideshow?
(check-expect (in-slideshow? "a" '()) #f)
(check-expect (in-slideshow? "maximizing" SLIDESHOW) #t)
(check-expect (in-slideshow? "soap" SLIDESHOW) #f)
(define (in-slideshow? str slideshow)
  (cond [(empty? slideshow) #f]
        [(cons? slideshow) (or (in-slide? str (first slideshow))
                               (in-slideshow? str (rest slideshow)))]))

;; in-slide? : String Slide -> Boolean
;; Is this string anywhere in the slide?
(check-expect (in-slide? "neat" SLIDE-1) #t)
(check-expect (in-slide? "neatest" SLIDE-1) #f)
(define (in-slide? str slide)
  (in-los? str (cons (slide-title slide) (append (slide-shown slide) (slide-hidden slide)))))

;; in-los? : String LoS -> Boolean
;; Is this string anywhere inside los?
(check-expect (in-los? "a" '()) #f)
(check-expect (in-los? "help" SLIDE-1-LOS) #t)
(check-expect (in-los? "helper" SLIDE-1-LOS) #f)
(define (in-los? str los)
  (cond [(empty? los) #f]
        [(cons? los) (or (string-contains? str (first los))
                         (in-los? str (rest los)))]))

;; total-characters : Slideshow -> Number
;; Total number of characters in the slideshow
(check-expect (total-characters '()) 0)
(check-expect (total-characters SLIDESHOW) 559)
(define (total-characters slideshow)
  (cond [(empty? slideshow) 0]
        [(cons? slideshow) (+ (total-characters/slide (first slideshow))
                              (total-characters (rest slideshow)))]))

;; total-characters/slide : Slide -> Number
;; Total characters in the slide
(check-expect (total-characters/slide SLIDE-1) 128)
(define (total-characters/slide slide)
  (total-characters/los
   (cons (slide-title slide) (append (slide-shown slide) (slide-hidden slide)))))

;; total-characters/los : LoS -> Boolean
;; Total characters in los
(check-expect (total-characters/los '()) 0)
(check-expect (total-characters/los (cons "bee" (cons "apple" '()))) 8)
(define (total-characters/los los)
  (cond [(empty? los) 0]
        [(cons? los) (+ (string-length (first los))
                        (total-characters/los (rest los)))]))