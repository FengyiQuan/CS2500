;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab10-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A GameOfLife is a [List-of [List-of Boolean]]
;; where the length of the outer list is the same as all of the inner lists
;; and represents alive (#t) and dead (#f) cells
(define TINY-GAME
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #f #f)
        (list #f #f #f #f)))

(define TINY-GAME-NEXT
  (list (list #f #f #f #f)
        (list #f #t #t #f)
        (list #f #t #t #f)
        (list #f #f #f #f)))

;; get-cell : Nat Nat GameOfLife-> Boolean
;; The value of the cell at y, x (looping around the ends of the list if necessary)
(check-expect (get-cell 0 0 TINY-GAME) #f)
(check-expect (get-cell 4 4 TINY-GAME) #f)
(check-expect (get-cell 1 2 TINY-GAME) #t)
(check-expect (get-cell 5 6 TINY-GAME) #t)
(define (get-cell y x gol)
  (list-ref (list-ref gol (modulo y (length gol))) (modulo x (length gol))))
    
;; neighbors : Nat Nat GameOfLife -> [List-of Boolean]
;; The neighbors of i, j in gol (i = row, j = column)
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

;; count-trues : [List-of Boolean] -> [List-of Boolean]
;; Count the number of true values
(check-expect (count-trues (list #t #f #t #f #t)) 3)
(define (count-trues lot) (length (filter identity lot)))

;; new-value/conway : Boolean [List-of Boolean] -> Boolean
;; The cell's new value by conway's rules
(check-expect (new-value/conway #f (append (make-list 3 #t) (make-list 5 #f))) #t)
(check-expect (new-value/conway #f (append (make-list 4 #t) (make-list 4 #f))) #f)
(check-expect (new-value/conway #t (append (make-list 3 #t) (make-list 5 #f))) #t)
(check-expect (new-value/conway #t (append (make-list 2 #t) (make-list 6 #f))) #t)
(check-expect (new-value/conway #t (append (make-list 4 #t) (make-list 4 #f))) #f)
#;(define (new-value/conway cell neighbors)
    (local [(define alive-neighbor-count (count-trues neighbors))]
      (or (and cell (or (= 2 alive-neighbor-count) (= 3 alive-neighbor-count)))
          (and (not cell) (= 3 alive-neighbor-count)))))

#|
;; next-grid : GameOfLife -> GameOfLife
;; Output the next grid
(check-expect (next-grid TINY-GAME) TINY-GAME-NEXT)
(define (next-grid gol)
  (local [;; row : Nat -> [List-of Boolean]
          ;; Output the ith row
          (define (row i)
            (local [;; cell : Nat -> Boolean
                    ;; The cell at i, j
                    (define (cell j)
                      (new-value/conway (get-cell i j gol)
                                        (neighbors i j gol)))]
              (build-list (length gol) cell)))]
    (build-list (length gol) row)))
|#

(define CELL-SIZE 5)
(define LIVE-CELL (color-frame "cadetblue" (square CELL-SIZE "solid" "seashell")))
(define DEAD-CELL (square CELL-SIZE "solid" "white"))

;; draw-game : GameOfLife -> Image
;; Draw the game of life
(check-expect (draw-grid TINY-GAME)
              (above (beside DEAD-CELL DEAD-CELL DEAD-CELL DEAD-CELL)
                     (beside DEAD-CELL LIVE-CELL LIVE-CELL DEAD-CELL)
                     (beside DEAD-CELL LIVE-CELL DEAD-CELL DEAD-CELL)
                     (beside DEAD-CELL DEAD-CELL DEAD-CELL DEAD-CELL)))
(define (draw-grid gol)
  (local [;; draw-row : [List-of Boolean] -> Image
          ;; Draw a row
          (define (draw-row lob)
            (local [;; draw-cell-next-to : Boolean Image -> Image
                    ;; Draw cell beside i
                    (define (draw-cell-next-to b i)
                      (beside (if b LIVE-CELL DEAD-CELL) i))]
              (foldr draw-cell-next-to empty-image lob)))
          ;; put-row : [List-of Boolean] Image -> Image
          ;; Put a row above i
          (define (put-row row i)
            (above (draw-row row) i))]
    (foldr put-row empty-image gol)))
#|
;; main : GameOfLife -> GameOfLife
;; Run conway's game of life
(define (main gol)
  (big-bang gol
    [on-tick next-grid 1/10]
    [to-draw draw-grid]))
(main 
 (list (list #f #f #f #f #f #f #f)
       (list #f #f #f #f #f #f #f)
       (list #f #f #f #f #f #f #f)
       (list #f #f #f #f #f #f #f)
       (list #f #f #f #f #t #t #t)
       (list #f #f #f #f #t #f #f)
       (list #f #f #f #f #f #t #f)))
|#

;; A FirstState is a [List-of (list Nat Nat)]
;; and represents a list of coordinates that are alive at the onset of of the game

;; initial-grid : Nat FirstState -> GameOfLife
;; A game of life with size grid-size with points in fs alive
(check-expect (initial-grid 4 (list (list 1 1)
                                    (list 1 2)
                                    (list 2 1)))
              TINY-GAME)
(define (initial-grid grid-size fs)
  (local [;; make-row : Nat -> [List-of Boolean]
          ;; Make row i
          (define (make-row i)
            (local [;; make-cell : Nat -> Boolean
                    ;; Make cell i, j
                    (define (make-cell j)
                      (member? (list i j) fs))]
              (build-list grid-size make-cell)))]
    (build-list grid-size make-row)))

;; main : Nat FirstState -> GameOfLife
;; Run conway's game of life
#;(define (main grid-size fs)
    (big-bang (initial-grid grid-size fs)
      [on-tick next-grid 1/10]
      [to-draw draw-grid]))

#;(main
   50
   '((20 20)
     (20 21)
     (21 20)
     (22 20)
     (20 24)
     (21 24)
     (22 24)
     (22 23)))

;; A CellUpdate is a [Boolean [List-of Boolean] -> Boolean]
;; and represents the rules by which a cell's new value is determined

; cell-update : [List-of [0, 8]] [List-of [0, 8]] -> CellUpdate
; Output a cell update function with "birth" numbers and "survival" numbers
(define (cell-update birth survival)
  (local [;; new-value : Boolean [List-of Boolean] -> Boolean
          ;; Given a cell's current state and its neighbors, output if it is alive or dead
          (define (new-value cell neighbors)
            (local [(define live-neighbors (count-trues neighbors))]
              (or (and (not cell) (member live-neighbors birth))
                  (and cell (member live-neighbors survival)))))]
    new-value))

(define new-value/conway (cell-update (list 3) (list 2 3)))

;; next-grid : CellUpdate GameOfLife -> GameOfLife
;; Output the next grid
(check-expect (next-grid new-value/conway TINY-GAME) TINY-GAME-NEXT)
(define (next-grid cell-update gol)
  (local [;; row : Nat -> [List-of Boolean]
          ;; Output the ith row
          (define (row i)
            (local [;; cell : Nat -> Boolean
                    ;; The cell at i, j
                    (define (cell j)
                      (cell-update (get-cell i j gol)
                                   (neighbors i j gol)))]
              (build-list (length gol) cell)))]
    (build-list (length gol) row)))

;; main : CellUpdate Nat FirstState -> GameOfLife
;; Run conway's game of life
(define (main cell-update grid-size fs)
  (local [;; next-grid/main : GameOfLife -> GameOfLife
          ;; The next grid, using cell-update as a rule
          (define (next-grid/main gol)
            (next-grid cell-update gol))]
    (big-bang (initial-grid grid-size fs)
      [on-tick next-grid/main 1/10]
      [to-draw draw-grid])))
(main
 (cell-update '(2) '())
 50
 '((20 20)
   (20 21)
   (21 20)
   (22 20)
   (20 24)
   (21 24)
   (22 24)
   (22 23)))