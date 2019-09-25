;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |2016 Midterm 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; Problem 1
;; A PB is one of:
;; - '()
;; - (cons String (cons Number PB))
;; Interpretation : A phone book such as
;;                  (cons "Alan" (cons 617738.1212 pb))
;; means "Alan"'s phone number is 617738.1212,
;; and the rest of the phone book is pb

;; remove-both : String PB -> PB
;; consumes a string n and a PB, returns a PB with all
;; occurrences of n and the following phone number removed.
(define PB-0 '())
(define PB-1 (cons "ABC" (cons 123456 PB-0)))
(define PB-2 (cons "asd" (cons 123 PB-1)))
(define PB-3 (cons "poi" (cons 234 PB-2)))
(check-expect (remove-both "ABC" PB-3) (cons "poi" (cons 234
                                                         (cons "asd" (cons 123 PB-0)))))
(check-expect (remove-both "ABC" PB-1) PB-0)
(define (remove-both s pb)
  (cond
    [(empty? pb) '()]
    [(cons? pb) (if (string=? s (first pb))
                    (remove-both s (rest (rest pb)))
                    (cons (first pb)
                          (cons (second pb) (remove-both s (rest (rest pb))))))]))


;; Problem 2
;; (a)
;; drop : [List-of Posn] -> [List-of Posn]
;; consumes and produces a list of Posns. Each Posn whose y coordinate is larger than 200 is removed.
;; All other y coordinates are increased by 3.
(define LOP-0 '())
(define LOP-1 (cons (make-posn 1 2) LOP-0))
(define LOP-2 (cons (make-posn 1 2001) LOP-1))
(define LOP-3 (cons (make-posn 2 1999) LOP-2))


;; (b)
(check-expect (drop LOP-0) LOP-0)
(check-expect (drop LOP-1) (list (make-posn 1 5)))
(check-expect (drop LOP-2) (list (make-posn 1 5)))
(check-expect (drop LOP-3) (list (make-posn 2 2002)
                                 (make-posn 1 5)))
(define (drop lop)
  (foldr remove-one '() lop))

;; remove-one : Posn [List-of Posn] -> [List-of Posn]
;; remove one posn if its coordinate is larger than 200
;; otherwise increased 3 to y coordinate
(check-expect (remove-one (make-posn 1 2) LOP-0) (list (make-posn 1 5)))
(define (remove-one posn lop)
  (if (> (posn-y posn) 2000)
      lop
      (cons (make-posn (posn-x posn)
                       (+ 3 (posn-y posn)))
            lop)))

;; (c)
(check-expect (drop-no-abstraction LOP-0) LOP-0)
(check-expect (drop-no-abstraction LOP-1) (list (make-posn 1 5)))
(check-expect (drop-no-abstraction LOP-2) (list (make-posn 1 5)))
(check-expect (drop-no-abstraction LOP-3) (list (make-posn 2 2002)
                                                (make-posn 1 5)))
(define (drop-no-abstraction lop)
  (cond
    [(empty? lop) '()]
    [(cons? lop) (append (remove-or-increase (first lop))
                         (drop-no-abstraction (rest lop)))]))

;; remove-or-increase : Posn -> [List-of Posn]
(define (remove-or-increase posn)
  (if (> (posn-y posn) 2000)
      '()
      (list (make-posn (posn-x posn)
                       (+ 3 (posn-y posn))))))

;; Problem 3
(define LEFT-WING "(")
(define RIGHT-WING ")")
; A Butterfly is one of:
; -- "body"
; -- (list LEFT-WING Butterfly RIGHT-WING)
(define B-0 "body")
(define B-1 (list LEFT-WING B-0 RIGHT-WING))
(define B-2 (list LEFT-WING B-1 RIGHT-WING))

;; count-wings : Butterfly -> Number
;; consumes a Butterfly and counts the pairs of wings that surround its body
(check-expect (count-wings B-0) 0)
(check-expect (count-wings B-1) 1)
(check-expect (count-wings B-2) 2)
(define (count-wings b)
  (cond
    [(string? b) 0]
    [(cons? b) (add1 (number-in-body (second b)))]))

;; number-in-body : Butterfly -> Number
(define (number-in-body b)
  (cond
    [(string? b) 0]
    [(cons? b) (count-wings b)]))

;; Problem 4
(define-struct bullets (loi))
(define-struct points (loi))
(define-struct item (low))

; A Word is one of:
(define EN-3 (make-points '()))
; -- String
; -- Enumeration
(define WORD-1 "abc")
(define WORD-2 "@#$%")
(define WORD-3 EN-3)

(define ITEM-1 (make-item (list WORD-1)))
(define ITEM-2 (make-item (list WORD-2)))

; An Enumeration is one of:
; -- (make-bullets LoI) ;; bulletized items
; -- (make-points LoI) ;; numbered points
(define EN-1 (make-bullets (list ITEM-2)))
(define EN-2 (make-points (list ITEM-1)))

; An LoI is a [List-of Item]

; An Item is a structure: (make-item LoW)

; An LoW is a [List-of Word]


; interpretation An Enumeration is a generic data
; representation of HTML, LateX, etc nested, itemized lists.

;; cleanse : Enumeration -> Enumeration
;; consumes an Enumeration and removes all bad strings.
(define (cleanse en)
  (cond
    [(bullets? en) (loi-helper (bullets-loi en))]
    [(points? en) (loi-helper (points-loi en))]))

; loi-helper : [List-of Item] -> [List-of Item]
(define (loi-helper loi)
  (map one-item loi))
  
; one-item : Item -> Item
(define (one-item i)
  (foldr remove-bad '() (item-low i)))

;m remove-bad : Word [List-of Word] -> [List-of Word]
(define (remove-bad w sofar)
  (cond
    [(string? w) (if (string=? w "@#$%")
                     sofar
                     (cons w sofar))]
    [else (cleanse w)]))


