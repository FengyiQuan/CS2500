;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |tree structure|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct person [name parent1 parent2])
;; A Person is one of:
;; - #false
;; - (make-person String Person Person)
;; Interpretation:
;; a person with a name and two biological parents or unknown person
(define PERSON-0 #f)
(define PERSON-1 (make-person "john smith" PERSON-0 PERSON-0))
(define PERSON-2 (make-person "peter smith" PERSON-0 PERSON-0))
(define PERSON-3 (make-person "jane smith" PERSON-1 PERSON-2))
(define PERSON-4 (make-person "nate-getting-fired" PERSON-3 PERSON-0))
#;
(define (person-temp p)
  (cond
    [(boolean? p) ...]
    [(person? p) ...
     (person-name p) ...
     (person-temp (person-parent1 p)) ...
     (person-temp (person-parent2 p)) ...]))

; Design a function tree-size that returns how many persons there are in a tree
;; tree-size : Person -> Number
(check-expect (tree-size PERSON-0) 0)
(check-expect (tree-size PERSON-1) 1)
(check-expect (tree-size PERSON-2) 1)
(check-expect (tree-size PERSON-3) 3)
(check-expect (tree-size PERSON-4) 4)
(define (tree-size p)
  (cond
    [(boolean? p) 0]
    [(person? p) (+ 1
                    (tree-size (person-parent1 p))
                    (tree-size (person-parent2 p)))]))

;; max-path : Person -> Number
;; returns the longest path from the person
(check-expect (max-path PERSON-0) 0)
(check-expect (max-path PERSON-1) 1)
(check-expect (max-path PERSON-2) 1)
(check-expect (max-path PERSON-3) 2)
(check-expect (max-path PERSON-4) 3)
(define (max-path p)
  (cond
    [(boolean? p) 0]
    [(person? p) (add1 (max
                        (max-path (person-parent1 p))
                        (max-path (person-parent2 p))))]))

;; Design a 3-Tree where every non-empty node has exactly 3 children
;; and a number

(define-struct 3tree [value c1 c2 c3])
;; A 3-Tree is one of:
;; - #f
;; - (make-3tree Number 3-Tree 3-Tree 3-Tree)
#;
(define (3tree-temp 3t)
  (cond
    [(boolean? st) ...]
    [(3tree? 3t) ...
     (3tree-value 3t) ...
     (3tree-temp (3tree-c1 3t)) ...
     (3tree-temp (3tree-c2 3t))
     (3tree-temp (3tree-c3 3t)) ... ]))

;; Design a WebPage, which has a title and links to other webpages

(define-struct webpage [title links])
; A WebPage is a (make-webpage String [List-of WebPage])
(define WP-1 (make-webpage "Google" '()))
(define WP-2 (make-webpage "CCIS" (list WP-1)))
(define WP-3 (make-webpage "NU" (list WP-1 WP-2)))
(define WP-4 (make-webpage "ND" (list WP-2)))
#;
(define (webpage-temp wp)
  ... (webpage-title wp) ... (lowp-temp (webpage-links wp)) ...)

#;
(define (lowp-temp lowp)
  (cond
    [(empty? lowp) ...]
    [(cons? lowp) ...
     (webpage-temp (first lowp)) ...
     (lowp-temp (rest lowp)) ...]))

;; Design exists? that takes a string and a webpage and returns
;; #t if that title exists as a webpage tilte
;; exists? : String WebPage -> Boolean
(check-expect (exists? "foo" WP-3) #f)
(check-expect (exists? "Google" WP-3) #t)
(check-expect (exists? "Google" WP-4) #t)
(define (exists? s wp)
  (local [;; exist-lowp? : String [List-of WebPage] -> Boolean
          ;; returns #t if any of the webpages have the string as title
          (define (exist-lowp? s lowp)
            (local [; helper : WebPage -> Boolean
                    (define (helper wp)
                      (exists? s wp))]
              (ormap helper lowp)))]
    (or (string=? s (webpage-title wp))
        (exist-lowp? s (webpage-links wp)))))




