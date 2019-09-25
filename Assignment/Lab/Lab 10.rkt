;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Lab 10
; A GameOfLife is a [List-of [List-of Boolean]]
; where the length of the outer list is the same as all of the inner lists
; and represents alive (#t) and dead (#f) cells
(define TINY-GAME
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #f #f)
        (list #f #f #f #f)))

;; Exercise (Reviewed) 1
(define NEXT-TINY-GAME
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #t #f)
        (list #f #f #f #f)))

;; Exercise (Reviewed) 2
; neighbors : Nat Nat GameOfLife -> [List-of Boolean]
; The neighbors of i, j in gol (i = row, j = column)
(check-expect (neighbors 1 1 TINY-GAME)
              (list #f #f #f
                    #f    #t
                    #f #t #f))
(check-expect (neighbors 3 1 TINY-GAME)
              (list #f #t #f
                    #f    #f
                    #f #f #f))
(define (neighbors i j gol)
  (list (get-cell (sub1 i) (sub1 j) gol)
        (get-cell (sub1 i) j        gol)
        (get-cell (sub1 i) (add1 j) gol)
        (get-cell i        (sub1 j) gol)
        (get-cell i        (add1 j) gol)
        (get-cell (add1 i) (sub1 j) gol)
        (get-cell (add1 i) j        gol)
        (get-cell (add1 i) (add1 j) gol)))
 
; get-cell : Nat Nat GameOfLife -> Boolean
; The value of the cell at y, x (looping around the ends of the list if necessary)
(check-expect (get-cell 0 0 TINY-GAME) #f)
(check-expect (get-cell 4 4 TINY-GAME) #f)
(check-expect (get-cell 1 2 TINY-GAME) #t)
(check-expect (get-cell 5 6 TINY-GAME) #t)
(define (get-cell y x gol)
  (list-ref (list-ref gol
                      (modulo y (length gol)))
            (modulo x (length gol))))

;; Exercise 3
;; num-#t : [List-of Boolean] -> Number
;; given a list of booleans returns the number of elements that are #t
(check-expect (num-#t (list)) 0)
(check-expect (num-#t (list #f #f #t #t)) 2)
(define (num-#t lob)
  (local [;; add1-if-true : Boolean Number -> Number
          (define (add1-if-true b n)
            (if (boolean=? #t b)
                (add1 n)
                n))]
    (foldr add1-if-true 0 lob)))

;; Exercise 4
;; new-value/conway : Boolean [List-of Boolean] -> Boolean
;; given a cell’s current value and the list of its neighbors’ current values,
;; outputs the new value of the cell
(check-expect (new-value/conway #f (list #f #f #f #t #t #t #t #t)) #f)
(check-expect (new-value/conway #f (list #f #f #f #t #t #t #f #f)) #t)
(check-expect (new-value/conway #t (list #f #f #f #t #t #t #t #t)) #f)
(define (new-value/conway b lob)
  (cond
    [(and (boolean=? b #t) (<= 2 (num-#t lob) 3)) #t]
          [(and (boolean=? b #f) (= (num-#t lob) 3)) #t]
          [else #f]))

;; Exercise 5
;; next-grid :  GameOfLife




