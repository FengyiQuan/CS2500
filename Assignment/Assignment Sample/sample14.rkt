;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample14) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

(define-struct leaf [])
(define-struct node [data left right])
; A [Tree X] is one of:
; - (make-leaf)
; - (make-node X [Tree X] [Tree X])
 
 
(define-struct family-member [name yob])
; A FamilyMember is a (make-family-member String Number)
; - where name is the family member's name
; - and yob is their year of birth
 
; An FT is a [Tree FamilyMember] and represents a family tree, with the youngest family member
; at the root.

;; Exercise 1

(define MATT (make-family-member "Matt" 1994))
(define RUTH (make-family-member "Ruth" 1960))
(define JEFF (make-family-member "Jeff" 1962))
(define BILL (make-family-member "Bill" 100))

(define TREE-0 (make-leaf))
(define TREE-1 (make-node MATT
                          (make-node RUTH
                                     (make-leaf)
                                     (make-leaf))
                          (make-node JEFF
                                     (make-leaf)
                                     (make-leaf))))

;; ft-temp : FT -> ?
(define (ft-temp ft)
  (cond [(leaf? ft) ...]
        [(node? ft) (... (family-member-temp (node-data ft))
                         (ft-temp (node-left ft))
                         (ft-temp (node-right ft)))]))


;; Exercise 2

;; ages : FT -> [List-of Number]
;; Return the ages of the people in the tree
(check-expect (ages TREE-0) '())
(check-expect (ages TREE-1) (list 24 58 56))
(define (ages ft)
  (cond [(leaf? ft) '()]
        [(node? ft)  (cons (get-age (node-data ft))
                           (append (ages (node-left ft))
                                   (ages (node-right ft))))]))

;; get-age : FamilyMember -> Number
;; Get the age of the given family member
(check-expect (get-age MATT) 24)
(check-expect (get-age RUTH) 58)
(define (get-age fm)
  (- 2018 (family-member-yob fm)))

;; Exercise 3

;; max-age : FT -> Number
;; Produces the maximum age of any family member in this family tree
(check-expect (max-age TREE-0) 0)
(check-expect (max-age TREE-1) 58)
(define (max-age ft)
  (cond [(leaf? ft) 0]
        [(node? ft)
         (max (get-age (node-data ft))
              (max-age (node-left ft))
              (max-age (node-right ft)))]))

;; Alternatively
#;(define (max-age ft)
    (cond [(leaf? ft) 0]
          [(node? ft)
           (apply max (ages ft))]))
;; But this builds up a list unnecessarily

;; Exercise 4

;; generations : FT -> Nat
;; Produces the number of generations displayed in the tree
(check-expect (generations TREE-0) 0)
(check-expect (generations TREE-1) 2)
(define (generations ft)
  (cond [(leaf? ft) 0]
        [(node? ft)
         (add1 (max (generations (node-left ft))
                    (generations (node-right ft))))]))

;; Exercise 5

;; anyone-named-after-ancestors? : FT -> Boolean
;; Has anyone been named after their ancestor?
(check-expect (ormap anyone-named-after-ancestors? (list TREE-0 TREE-1)) #f)
(check-expect (anyone-named-after-ancestors? (make-node (make-family-member "Ruth" 2040)
                                                        TREE-0
                                                        TREE-1))
              #t)
(define (anyone-named-after-ancestors? ft)
  (cond [(leaf? ft) #f]
        [(node? ft) (or (anyone-named? (node-data ft)
                                       (node-left ft))
                        (anyone-named? (node-data ft)
                                       (node-right ft))
                        (anyone-named-after-ancestors? (node-left ft))
                        (anyone-named-after-ancestors? (node-right ft)))]))

;; anyone-named? : FamilyMember FT -> Boolean
;; Has anyone been named this?
(check-expect (anyone-named? RUTH TREE-1) #t)
(check-expect (anyone-named? BILL TREE-1) #f)
(define (anyone-named? fm ft)
  (cond [(leaf? ft) #false]
        [(node? ft)
         (or (same-name? (node-data ft) fm)
             (anyone-named? fm (node-left ft))
             (anyone-named? fm (node-right ft)))]))

;; same-name? : FamilyMember FamilyMember -> Boolean
;; Do these two family members have the same name?
(check-expect (same-name? RUTH MATT) #false)
(check-expect (same-name? MATT MATT) #true)
(define (same-name? fm1 fm2)
  (string=? (family-member-name fm1) (family-member-name fm2)))