;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname arrangement) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
;; An Arrangement is a [List-of 1String]
; Interpretation: A particular arrangement of Scrabble titles
(define ARRANGEMENT-1 (list "a" "z" "c"))

#;
(define (arr-temp arr)
  (cond
    [(empty? arr) ...]
    [(cons? arr) ...
     (first arr) ...
     (arr-temp (rest arr)) ...]))

; Design a function scrabble-words that, given
; a list of tiles (Arrangement) produces all valid
; scrabble words from those tiles.

; scrabble-words : Arrangement -> [List-of String]
(check-expect (scrabble-words (list "a" "c" "t"))
              (list "act" "cat" "a" "at" "ta"))
(check-expect (scrabble-words (list "b" "e" "e" "z"))
              (list "be" "bee" "zee"))
(define (scrabble-words arr)
  (filter in-dictionary? (map arr->string (make-all-the-arrangements arr))))


;; list-same? : (X) [List-of X] [List-of X] -> Boolean
;; have all the same contents without regard to order
(check-expect (list-same? (list 1 2 3) (list 3 2 1)) #t)
(check-expect (list-same? (list 1 2 3 4) (list 3 2 1)) #f)
(check-expect (list-same? (list 1 2 3) (list 3 2 1 4)) #f)
(define (list-same? l1 l2)
  (local [; in-list1? : X -> Boolean
          (define (in-list1? x)
            (member? x l1))
          ; in-list2? : X -> Boolean
          (define (in-list2? x)
            (member? x l2))]
    (and (amdmap in-list2? l1)
         (andmap in-list1? l2))))

;; arr->string : Arrangement -> String
;; converts a list of 1String to a string
(check-expect (arr->string '()) "")
(check-expect (arr->string (list "a" "b" "c")) "abc")
(define (arr->string arr)
  (implode arr))

(define DICTIONARY (read-lines "wordslower.txt"))
;; in-dictionary? : String -> Boolean
;; is the word in the dictionary
(check-expect (in-dictionary? "asdfgaergasrg") #f)
(check-expect (in-dictionary? "cat") #t)
(define (in-dictionary? w)
  (member? w DICTIONARY))

;; make-all-the-arrangements : Arrangement -> [List-of Arrangement]
;; gets all possible combinations of the letters in the arrangement
(check-expect (make-all-the-arrangements '()) (list '()))
(check-expect (make-all-the-arrangements (list "a"))
              (map explode (list "" "a")))
(check-expect (make-all-the-arrangements (list "a" "b"))
              (map explode (list "" "a" "b" "ab" "ba")))
(check-expect (make-all-the-arrangements (list "a" "b" "c"))
              (map explode (list "" "c"
                                 "a" "ac" "ca"
                                 "b" "bc" "cb"
                                 "ab" "abc" "acb" "cab"
                                 "ba" "bac" "bca" "cba")))
(define (make-all-the-arrangements arr)
  (cond
    [(empty? arr) (list '())]
    [(cons? arr)
     (insert-everywhere/list-of-arrangements
      (first arr)
      (make-all-the-arrangements (rest arr)))]))

;; insert-everywhere/list-of-arrangements : 1String [LIst-of Arrangements] -> [List-of Arrangements]
;; create all the arrangements by adding a 1String everywhere
(check-expect (insert-everywhere/list-of-arrangements "b")
              (map explode (list "" "a"))
              (map explode (list "" "a" "b" "ab" "ba")))
(define (insert-everywhere/list-of-arrangements 1s loarr)
  (cond
    [(empty? loarr) '()]
    [(cons? loarr) (append
                    (list (first loarr))
                    (insert-everywhere/arrangement (first loarr) 1s)
                    (insert-everywhere/list-of-arrangements 1s (rest loarr)))]))

;; insert-everywhere/arrangement : Arrangement 1String -> [LIst-of Arrangement]
;; put the 1String in all the places in the arrangement
(check-expect (insert-everywhere/arrangement (list "a" "b") "c")
              (map explode (list "cab" "acb" "abc")))
(define (insert-everywhere/arrangement arr 1s)
  (local [;; lisert-at-x : Nat
          (define (lisert-at-x index)
            (make-an-arrangement arr 1s index))]
    (build-list (add1 (length arr)) lisert-at-x)))

;; make-an-arrangement : Arrangement 1String Nat -> Arrangement
;' puts the 1String at a particular index in the arrangement
(define (make-an-arrangement arr 1s index)
  (cond
    [(zero? index) (cons 1s arr)]
    [else (cons
           (first arr)
           (make-an-arrangement (rest arr) 1s  (sub1 index)))]))
