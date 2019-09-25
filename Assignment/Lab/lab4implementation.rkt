;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname lab4-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A PRS (PetRockStorange) is one of:
;; - "King Paimon"
;; - (make-bag String Number PRS)
(define-struct bag [color size contents])
;; and represents the pet rock
;; or a bag containing it (and possibly other bags) with a specific color and size

(define PRS-0 "King Paimon")
(define PRS-1 (make-bag "green" 10 PRS-0))
(define PRS-2 (make-bag "red" 20 PRS-1))
(define PRS-3 (make-bag "blue" 15 PRS-2))

;; petrockstorage-temp : PRS -> ?
(define (petrockstorage-temp prs)
  (cond [(string? prs) ...]
        [(bag? prs) (... (bag-color prs)
                         (bag-size prs)
                         (petrockstorage-temp (bag-contents prs)))]))

;; biggest-size-bag : PRS -> Number
;; Return the biggest size bag in the storage system (or 0 if no bags)
(check-expect (biggest-size-bag PRS-0) 0)
(check-expect (biggest-size-bag PRS-3) 20)
(define (biggest-size-bag prs)
  (cond [(string? prs) 0]
        [(bag? prs) (max (bag-size prs) (biggest-size-bag (bag-contents prs)))]))

;; contains? : PRS String -> Boolean
;; Does the PRS contain a bag of color s?
(check-expect (contains? PRS-0 "green") #f)
(check-expect (contains? PRS-3 "green") #t)
(check-expect (contains? PRS-3 "purple") #f)
(define (contains? prs s)
  (cond [(string? prs) #f]
        [(bag? prs) (or (string=? (bag-color prs) s)
                        (contains? (bag-contents prs) s))]))

;; grow-bag-sizes : PRS -> PRS
;; Grow all of the bag sizes by 1
(check-expect (grow-bag-sizes PRS-0) PRS-0)
(check-expect (grow-bag-sizes PRS-2)
              (make-bag "red" 21
                        (make-bag "green" 11 PRS-0)))
(define (grow-bag-sizes prs)
  (cond [(string? prs) prs]
        [(bag? prs) (make-bag (bag-color prs)
                              (add1 (bag-size prs))
                              (grow-bag-sizes (bag-contents prs)))]))

;; discard-big-bags : PRS Number -> PRS
;; Discard bags bigger than n
(check-expect (discard-big-bags PRS-0 5) PRS-0)
(check-expect (discard-big-bags PRS-3 11) PRS-1)
(define (discard-big-bags prs n)
  (cond [(string? prs) prs]
        [(bag? prs) (if (> (bag-size prs) n)
                        (discard-big-bags (bag-contents prs) n)
                        (make-bag (bag-color prs)
                                  (bag-size prs)
                                  (discard-big-bags (bag-contents prs) n)))]))
  
;; well-stored? : PRS -> Boolean
;; Do the contents always shrink in size?
(check-expect (well-stored? PRS-0) #t)
(check-expect (well-stored? PRS-2) #t)
(check-expect (well-stored? PRS-3) #f)
(define (well-stored? prs)
  (cond [(string? prs) #t]
        [(bag? prs) (and (> (bag-size prs) (biggest-size-bag (bag-contents prs)))
                         (well-stored? (bag-contents prs)))]))

;; A Nat  is one of:
;; - 0
;; - (add1 Nat)

(define NAT-0 0)
(define NAT-1 (add1 NAT-0))
(define NAT-2 (add1 NAT-1))

;; nat-temp : Nat -> ?
(define (nat-temp n)
  (cond [(zero? n) ...]
        [(positive? n) (... (nat-temp (sub1 n)))]))

;; double : Nat -> Nat
;; Double a nat
(check-expect (double 0) 0)
(check-expect (double 10) 20)
(define (double n)
  (cond [(zero? n) 0]
        [(positive? n) (add1 (add1 (double (sub1 n))))]))

;; even?/nat : Nat -> Nat
;; Is this nat even?
(check-expect (even?/nat 0) #t)
(check-expect (even?/nat 11) #f)
(check-expect (even?/nat 12) #t)
(define (even?/nat n)
  (cond [(zero? n) #t]
        [(positive? n) (not (even?/nat (sub1 n)))]))

;; nat+ : Nat Nat -> Nat
;; a+b
(check-expect (nat+ 0 12) 12)
(check-expect (nat+ 3 12) 15)
(define (nat+ a b)
  (cond [(zero? a) b]
        [(positive? a) (add1 (nat+ (sub1 a) b))]))

;; nat* : Nat Nat -> Nat
;; a*b
(check-expect (nat* 0 12) 0)
(check-expect (nat* 3 12) 36)
(define (nat* a b)
  (cond [(zero? a) 0]
        [(positive? a) (nat+ b (nat* (sub1 a) b))]))

;; factorial : Nat -> Nat
;; The factorial of n
(check-expect (factorial 0) 1)
(check-expect (factorial 5) 120)
(define (factorial n)
  (cond [(zero? n) 1]
        [(positive? n) (nat* n (factorial (sub1 n)))]))

;; draw-circle : Nat -> Image
;; Draw a circle of size r, red if even, otherwise black
(check-expect (draw-circle 2) (circle 2 "solid" "red"))
(check-expect (draw-circle 3) (circle 3 "solid" "black"))
(define (draw-circle r)
  (circle r "solid" (if (even? r) "red" "black")))

;; draw-circles : Nat -> Image
;; Draw concentric circles
(check-expect (draw-circles 0) empty-image)
(check-expect (draw-circles 2)
              (overlay empty-image (draw-circle 1) (draw-circle 2)))
(define (draw-circles n)
  (cond [(zero? n) empty-image]
        [else (overlay (draw-circles (sub1 n)) (draw-circle n))]))

;; draw-scene : Nat -> Image
;; Draw n concentric circles on a 200x200 scene
(check-expect (draw-scene 2) (overlay (draw-circle 1)
                                      (draw-circle 2)
                                      (empty-scene 200 200)))
(define (draw-scene n)
  (overlay (draw-circles #;(modulo n 100) (radius n)) (empty-scene 200 200)))

;; radius : Nat -> Nat
;; The radius for timestamp n
(check-expect (radius 99) 99)
(check-expect (radius 100) 100)
(check-expect (radius 101) 99)
(define (radius n)
  (cond [(even? (quotient n 100)) (modulo n 100)]
        [else (- 100 (modulo n 100))]))

(animate draw-scene)