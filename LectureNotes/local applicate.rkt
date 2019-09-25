;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |local applicate|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; identity!!!!!!!!!!!!!!
;; [X] Nat [Nat -> X] -> [List-of X]
;; Applies f to the first n natural numbers
;; (build-list n f) == (list (f 0) (f 1) (f 2) ... (f (sub1 n)))

;; Design a function which computes the first n double-squares,
;; or numbers that are twice as large as perfect squares.

;; double-squares : Number -> [List-of Number]
(check-expect (double-squares 4) (list 0 2 8 18))
(define (double-squares n)
  (build-list n sqr-2))

;; sqr-2 : Number -> Number
(define (sqr-2 n)
  (* 2 (sqr n)))

;--------------------------------------------
;; Design add-3-to-all, which accepts a [List-of Number] and adds 3 to every one.

;; add-3-to-all : [List-of Number] -> [List-of Number]
;; adds 3 to ALL the numbers

(check-expect (add-3-to-all '()) '())
(check-expect (add-3-to-all (list 1 2 3)) (list 4 5 6))

(define (add-3-to-all lon)
  (local [; add3 : Number -> Number
          ; adds 3 to a number
          ; Given 0, returns 3
          ; Given 5, returns 8
          (define (add3 n)
            (+ n 3))]
    (map add3 lon)))
; ------------------------------------------
;; Design a funciton USD-to-EUR that accepts a [List-of Number]
; representing prices in USD and a Nubmer representing the
; USD-EUR exchange rate, and converts each amount to EUR

;; USD-to-EUR : [List-of Number] Number -> [List-of Number]

(check-expect (USD-to-EUR (list 1 2 3) 2.0) (list 2 4 6))

(define (USD-to-EUR lon conversion)
  (local [; eur : Number -> Number
          ; converts USD to EUR
          ; Given 1 (and conversion=2), returns 2
          (define (eur usd)
            (* usd conversion))]
    (map eur lon)))
;-----------------------------------------------
; Design the function shortest-string that accepts a [List-of String]
; and returns the (first) shortest one.
; If the list is empty, it should returns #f

; A StringOrFalse (SoF) is one of:
; - #f
; - String

(define SOF-1 #f)
(define SOF-2 "jazz")

#;
(define (sof-temp sof)
  (cond
    [(boolean? sof) ...]
    [(string? sof) ...]))

; shortest-string : [List-of String] -> SoF

(check-expect (shortest-string (list "a" "b" "c")) "a")
(check-expect (shortest-string '()) #false)
(check-expect (shortest-string (list "abc" "def" "g" "hij")) "g")

(define (shortest-string sof)
  (local [; shortest : String SoF -> SoF
          ; gets the shortest so far
          ; Given "a" "b", should return "a"
          ; Given "b" "bb", should return "a"
          ; Given "a" #f, should return "a"
          ; Given "aa" "b", should return "b"
          (define (shortest s sof)
            (cond
              [(boolean? sof) s]
              [(string? sof) (if (> (string-length s) (string-length sof)) sof s)]))]
    (foldr shortest #f sof)))

;-------------------------------------------------------------
; Design the function slope that accepts two Posns
; and returns a Number representing the slope of the line
; defined by those two points
(check-expect (slope (make-posn 3 3) (make-posn 0 0)) 1)

(define (slope p1 p2)
  (local [(define RISE (- (posn-y p2) (posn-y p1)))
          (define RUN (- (posn-x p2)(posn-x p1)))]
    (/ RISE RUN)))

;----------------------------------------------------------------
; Design the function mymax that accepts a non-empty list of Numbers
; and returns the biggest

; A Non-Empty-List-of-Numbers (NeLoN) is one of:
; - (cons Number '())
; - (cons Number NeLoN)
(define NELON-1 (list 0 1 '()))
(define NELON-2 (list 0 1 2 3 '()))
#;
(define (nelon-temp nelon)
  (cond
    [(empty? (rest nelon)) ... (first nelon) ...]
    [(cons? nelon) (first nelon)
                   (nelon-temp (rest nelon)) ...]))
; mymax : NeLoN -> Number
; fins the max!
(check-expect (mymax NELON-1) 1)
(check-expect (mymax NELON-2) 3)
(define (mymax nelon)
  (cond
    [(empty? (rest nelon)) (first nelon)]
    [(cons? nelon) (if (> (first nelon) (mymax (rest nelon)))
                       (first nelon)
                       (mymax (rest nelon)))]))