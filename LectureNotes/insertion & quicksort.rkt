;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |insertion & quicksort|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
; 2500sort : [List-of Number] -> [List-of Number]
; sort the input list ascending
(check-expect (2500sort (list 1 2 3)) (list 1 2 3))
(check-expect (2500sort (list 3 2 1)) (list 1 2 3))
(check-expect (2500sort (list 79 1 2 3 3 -4)) (list -4 1 2 3 3 79))

#;(define (2500sort lon) ; insertion sort
    (cond
      [(empty? lon) '()]
      [(cons? lon)
       (local [;; place-it-in : Number [List-of Number] -> [List-of Number]
               (define (place-it-in n slon)
                 (cond
                   [(empty? slon) (list n)]
                   [(cons? slon)
                    (if (< n (first slon))
                        (cons n slon)
                        (cons (first slon)
                              (place-it-in n (rest slon))))]))]
         (place-it-in
          (first lon)
          (2500sort (rest lon))))]))

; 2500sort : [List-of Number] -> [List-of Number]
; sort the input list ascending
; Termination : we are always removing the pivot
;               before recurring and so the list of numbers
;               decreases in size at each step

(define (2500sort lon) ; quicksort
  (cond
    [(empty? lon) '()]
    [(cons? lon)
     (local [(define PIVOT (first lon))
             ;; less-than-pivot? : Number -> Boolean
             (define (less-than-pivot? n)
               (< n PIVOT))
             
             ;;greater-than-pivot? : Number -> Boolean
             (define (greater-than-pivot? n)
               (>= n PIVOT))
             
             (define SORTED-LEFT
               (2500sort (filter less-than-pivot? (rest lon))))
              (define SORTED-RIGHT
               (2500sort (filter greater-than-pivot? (rest lon))))]
       (append
        SORTED-LEFT
        (list PIVOT)
        SORTED-RIGHT))]))