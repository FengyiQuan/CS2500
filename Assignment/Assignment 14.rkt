;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 14|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])
(define TREE-0 (make-leaf))
 
(define-struct family-member [name yob])
; A FamilyMember is a (make-family-member String Number)
; - where name is the family member's name
; - and yob is their year of birth
(define FM-1 (make-family-member "sean" 1000))
(define FM-2 (make-family-member "nel" 1999))
(define FM-3 (make-family-member "wil" 2000))
(define FM-4 (make-family-member "sean" 2001))
#;
(define (family-member-temp fm)
  (family-member-name fm) ... (family-member-yob fm) ...)

; An FT is a [Tree FamilyMember] and represents a family tree, with the youngest
; family member at the root.
#;
(define (tree-temp t)
  (cond
    [(leaf? t) ...]
    [(node? t) ... (family-member-temp (node-data t)) ...
               (tree-temp (node-left t)) ...
               (tree-temp (node-right t)) ...]))

;; Exercise 1
;; Examples:
(define FT-1 (make-node FM-1 TREE-0 TREE-0))
(define FT-2 (make-node FM-2 TREE-0 TREE-0))
(define FT-3 (make-node FM-3 FT-1 FT-2))
(define FT-4 (make-node FM-4 TREE-0 FT-3))
(define FT-5 (make-node FM-3 TREE-0 TREE-0))

;; Exercise 2
;; everyone-age : FT -> [List-of Number]
;; returns the list of everyone’s age in a FT
(check-expect (everyone-age FT-1) (list 1018))
(check-expect (everyone-age FT-2) (list 19))
(check-expect (everyone-age FT-3) (list 18 1018 19))
(check-expect (everyone-age FT-4) (list 17 18 1018 19))
(define (everyone-age t)
  (cond
    [(leaf? t) '()]
    [(node? t) (append (list (one-age (node-data t)))
                       (everyone-age (node-left t))
                       (everyone-age (node-right t)))]))

;; one-age : FamilyMember -> Number
;; returns age of one family member
(check-expect (one-age FM-1) 1018)
(check-expect (one-age FM-2) 19)
(check-expect (one-age FM-3) 18)
(check-expect (one-age FM-4) 17)
(define (one-age fm)
  (- 2018 (family-member-yob fm)))

;; Exercise 3
;; max-age : FT ->  Number
;; produces the maximum age of any family member in a given FT
(check-expect (max-age FT-1) 1018)
(check-expect (max-age FT-2) 19)
(check-expect (max-age FT-3) 1018)
(check-expect (max-age FT-4) 1018)
(check-expect (max-age FT-5) 18)
(define (max-age t)
  (cond
    [(leaf? t) 0]
    [(node? t) (max (one-age (node-data t))
                    (max-age (node-left t))
                    (max-age (node-right t)))]))

;; Exercise 4
;; generation-number : FT -> Number
;; produces the number of generations shown by the tree
(check-expect (generation-number FT-1) 1)
(check-expect (generation-number FT-2) 1)
(check-expect (generation-number FT-3) 2)
(check-expect (generation-number FT-4) 3)
(define (generation-number t)
  (cond
    [(leaf? t) 0]
    [(node? t) (max (add1 (generation-number (node-left t)))
                    (add1 (generation-number (node-right t))))]))

;; Exercise 5
;; after-ancestor? : FT -> Boolean
;; determines if anyone has been named after their ancestor in a FT
(check-expect (after-ancestor? FT-1) #f)
(check-expect (after-ancestor? FT-2) #f)
(check-expect (after-ancestor? FT-3) #f)
(check-expect (after-ancestor? FT-4) #t)
(define (after-ancestor? t)
  (cond
    [(leaf? t) #f]
    [(node? t) (or (same-name? (node-data t) (node-left t))
                   (same-name? (node-data t) (node-right t))
                   (after-ancestor? (node-left t))
                   (after-ancestor? (node-right t)))]))

;; same-name? : FamilyMember FT -> Boolean
;; determine if a family member has been named after his ancestor
(check-expect (same-name? FM-1 FT-1) #t)
(check-expect (same-name? FM-1 FT-2) #f)
(check-expect (same-name? FM-3 FT-3) #t)
(check-expect (same-name? FM-4 FT-4) #t)

(define (same-name? fm t)
  (cond
    [(leaf? t) #f]
    [(node? t) (or (string=? (one-name fm) (one-name (node-data t)))
                   (same-name? fm (node-left t))
                   (same-name? fm (node-right t)))]))

;; one-name : FamilyMember -> String
;; returns the name of a family member
(check-expect (one-name FM-1) "sean")
(check-expect (one-name FM-2) "nel")
(check-expect (one-name FM-3) "wil")
(check-expect (one-name FM-4) "sean")
(define (one-name fm)
  (family-member-name fm))
