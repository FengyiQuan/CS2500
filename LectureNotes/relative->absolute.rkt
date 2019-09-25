;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname relative->absolute) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; relative->absolute : [List-of Number] -> [List-of Number]
;; convert them to a list of absolute distances from the first city
(check-expect (relative->absolute (list)) '())
(check-expect (relative->absolute (list 20)) (list 20))
(check-expect (relative->absolute (list 50 20)) (list 50 70))
(check-expect (relative->absolute (list 10 50 20)) (list 10 60 80))

#;(define (relative->absolute lon)
    (cond
      [(empty? lon) '()]
      [(cons? lon) (cons (first lon)
                         (add-num-to-each (first lon) (relative->absolute (rest lon))))]))

;; add-num-to-each : Number [List-of Number] -> [List-of Number]
;; adds a number to all the numbers in the list
(check-expect (add-num-to-each 10 (list 50 70)) (list 60 80))
(define (add-num-to-each n lon)
  (local [;; add-n : Number -> Number
          ;; adds n to each number
          (define (add-n num)
            (+ n num))]
    (map add-n lon)))

(define (relative->absolute lon)
  (local [;; relative->absolute/a : [List-of Number] Number -> [List-of Number]
          ;; Accumulator: keeps the current sum of the previous numbers in the list
          (define (relative->absolute/a lon tally)
            (cond
              [(empty? lon) '()]
              [(cons? lon) 
               (cons (+ tally (first lon))
                     (relative->absolute/a (rest lon) (+ tally (first lon))))]))]
    (relative->absolute/a lon 0)))

;; Accumulator: template
#;(define (foo x y z)
    (local [; foo/a : x y z acc
            (define (foo/a x y z acc
                           ...))]
      (foo/a x y z acc0)))

;; connected? : String String Wiki -> Boolean
;; Determines whether or not the two pages have a path between them
(define (connected? page1 page2 wiki)
  (local [;; connected?/a : String String Wiki [List-of String] -> Boolean
          ;; Accumulator: a list of the pages previously visited
          (define (connected?/a page1 page2 wiki visited)
            (cond
              [(string=? page1 page2) #t]
              [(member? page1 visited) #f]
              [else (local [; gets-there? : String -> Boolean
                            (define (gets-there? link)
                              (connected?/a link page2 wiki (cons page1 visited)))]
                            
                      (ormap gets-there? (lookup page1 wiki)))]))]
    (connected?/a page1 page2 wiki '())))

























