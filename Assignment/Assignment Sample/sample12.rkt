;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample12) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

;; Exercise 1

;; remove-punctuation : String -> String
;; Remove . or , or ; from the end of a word
(check-expect (remove-punctuation "") "")
(check-expect (remove-punctuation "apple.") "apple")
(check-expect (remove-punctuation "apple;") "apple")
(check-expect (remove-punctuation "apple,") "apple")
(check-expect (remove-punctuation "apple") "apple")
(define (remove-punctuation s)
  (cond [(string=? s "") s]
        [(member (substring s (sub1 (string-length s))) (list ";" "." ","))
         (substring s 0 (sub1 (string-length s)))]
        [else s]))

;; Exercise 2

;; process : String -> String
;; Processes a string for the word count problem
(check-expect (process "Apple.") "apple")
(define process (compose string-downcase remove-punctuation))

;; Exercise 3

;; compound-word? : String -> Boolean
;; Is this a compound-word?
(check-expect (compound-word? "dream-sages") #t)
(check-expect (compound-word? "dreamsages") #f)
(define (compound-word? s)
  (string-contains? "-" s))

;; An [NEList-of X] (Non-Empty List of X) is one of:
;; - (cons X '())
;; - (cons X [NEList-of X])

;; bin : [X X -> Boolean] [List-of X] -> [List-of [NEList-of X]]
;; Bin lox by matches?
(check-expect (bin = '()) '())
(check-expect (bin = (list 2 3 4 2)) (list (list 2 2) (list 4) (list 3)))
(define (bin matches? lox)
  (local [;; find-spot-for-x : X [List-of [NEList-of X]] -> [List-of [NEList-of X]]
          ;; Find the spot for x
          (define (find-spot-for-x x bins)
            (find-spot x matches? bins))]
    (foldr find-spot-for-x '() lox)))

;; find-spot : X [X X -> Boolean] [List-of [NEList-of X]] -> [List-of [NEList-of X]]
;; Find where x belonds and place it in the appropriate list (or give it its own)
(check-expect (find-spot 2 = '()) (list (list 2)))
(check-expect (find-spot 2 = (list (list 3) (list 2) (list 4))) (list (list 3) (list 2 2) (list 4)))
(define (find-spot x matches? bins)
  (cond [(empty? bins) (list (list x))]
        [(cons? bins) (if (matches? x (first (first bins)))
                          (cons (cons x (first bins))
                                (rest bins))
                          (cons (first bins)
                                (find-spot x matches? (rest bins))))]))

;; Exercise 4

;; unique-compound-words : [List-of String] -> [List-of String]
;; Produce the unique compound strings in los (all lowercase, ignore punctuation)
(check-expect (unique-compound-words '()) '())
(check-expect (unique-compound-words '("apple-bee;" "Apple-bee." "foo-bar" "bla"))
              '("foo-bar" "apple-bee"))
(define (unique-compound-words los)
  (map first (bin string=? (map process (filter compound-word? los)))))

(define UNIQUE-COMPOUND-WORDS (unique-compound-words (read-words "exoblivione.txt")))

;; A RawInput is a (list String String String String String String String)
;; and represents the x1, y1, z1, x2, y2, z2, and confidence level of a line in 3d space

;; A NumericInput is a (list Number Number Number Number Number Number [0, 1])
;; and represents the x1, y1, z1, x2, y2, z2, and confidence level of a line in 3d space

;; raw->numeric : RawInput -> NumericInput
;; Convert raw input to numeric input
(check-expect (raw->numeric (list "0" "1" "2" "3" "4" "5" ".4")) (list 0 1 2 3 4 5 .4))
(define (raw->numeric raw)
  (map string->number raw))

;; read-in-segements : String -> [List-of NumericInput]
;; Read in the line segments contained in s and map to numeric input
(define (read-in-segements s)
  (read-csv-file/rows s raw->numeric))

;; Exercise 5

;; keep-high-confidence-level : [List-of NumericInput] -> [List-of NumericInput]
;; Only keep the confidence level of .2 or higher
(check-expect (keep-high-confidence-level
               (list (list 0 1 2 3 4 5 .4)
                     (list 0 1 2 3 4 5 .1)))
              (list (list 0 1 2 3 4 5 .4)))
(define (keep-high-confidence-level loni)
  (local [;; high-enough? : NumericInput -> Boolean
          ;; Is the confidence level high enough?
          (define (high-enough? l)
            (>= (seventh l) .2))]
    (filter high-enough? loni)))

;; Exercise 6

;; A LineSegment is a (make-lineseg 3D 3D)
(define-struct lineseg [p1 p2])
;; and represents a line segment in 3d space from p1 to p2

;; A 3D is a (make-3d Number Number Number)
(define-struct 3d [x y z])
;; and represents a point in 3d space

;; Exercise 7

;; to-linseg : NumericInput -> LineSegment
;; From numeric input to a line segment
(check-expect (to-linseg (list 0 1 2 3 4 5 .4))
              (make-lineseg (make-3d 0 1 2) (make-3d 3 4 5)))
(define (to-linseg ni)
  (make-lineseg (make-3d (first ni) (second ni) (third ni))
                (make-3d (fourth ni) (fifth ni) (sixth ni))))

;; Exercise 8

;; flip-lineseg : LineSegment -> LineSegment
;; Flip the lineseg
(check-expect (flip-lineseg (make-lineseg (make-3d 0 1 2) (make-3d 3 4 5)))
              (make-lineseg (make-3d 3 4 5) (make-3d 0 1 2)))
(define (flip-lineseg lineseg)
  (make-lineseg (lineseg-p2 lineseg) (lineseg-p1 lineseg)))

;; Exercise 9

;; attached-at-p1? : LineSegment LineSegment -> Boolean
;; Are the two attached at p1?
(check-expect (attached-at-p1? (make-lineseg (make-3d 3 4 5) (make-3d 0 1 2))
                               (make-lineseg (make-3d 3 4 5) (make-3d 0 1 0)))
              #t)
(check-expect (attached-at-p1? (make-lineseg (make-3d 3 4 5) (make-3d 0 1 2))
                               (make-lineseg (make-3d 0 1 0) (make-3d 3 4 5)))
              #f)
(define (attached-at-p1? ls1 ls2)
  (equal? (lineseg-p1 ls1) (lineseg-p1 ls2)))

;; Exercise 10

;; bin-linesegs : [List-of LineSegment] -> [List-of [NEList-of LineSegment]]
;; Bin linesegements by where they pair
(check-expect (bin-linesegs
               (list
                (make-lineseg (make-3d 0 0 0) (make-3d 1 1 1))
                (make-lineseg (make-3d 2 2 2) (make-3d 1 1 1))
                (make-lineseg (make-3d 2 2 2) (make-3d 0 0 0))))
              (list
               (list (make-lineseg (make-3d 0 0 0) (make-3d 1 1 1))
                     (make-lineseg (make-3d 0 0 0) (make-3d 2 2 2)))
               (list (make-lineseg (make-3d 1 1 1) (make-3d 0 0 0))
                     (make-lineseg (make-3d 1 1 1) (make-3d 2 2 2)))
               (list (make-lineseg (make-3d 2 2 2) (make-3d 1 1 1))
                     (make-lineseg (make-3d 2 2 2) (make-3d 0 0 0)))))
(define (bin-linesegs linesegs)
  (bin attached-at-p1? (append linesegs (map flip-lineseg linesegs))))

;; Exercise 11

;; pairs : [List-of X] -> Number
;; How many pairs could be made from this list
(check-expect (pairs '()) 0)
(check-expect (pairs (list 2)) 0)
(check-expect (pairs (list 1 2 3)) 3)
(define (pairs lox)
  (* (length lox) (sub1 (length lox)) 1/2))

;; Exercise 12

;; total-pairs : [List-of LineSegment] -> Number
;; Total number of line segments that are attached at the ends
(check-expect (total-pairs (list
                            (make-lineseg (make-3d 0 0 0) (make-3d 1 2 3))
                            (make-lineseg (make-3d 0 0 0) (make-3d 1 1 1))
                            (make-lineseg (make-3d 2 2 2) (make-3d 1 1 1))
                            (make-lineseg (make-3d 2 2 2) (make-3d 0 0 0))))
              5)
(define (total-pairs los)
  (foldr + 0 (map pairs (bin-linesegs los))))

;; Exercise 13

(define NUMBER-OF-PAIRS
  (total-pairs
   (map to-linseg
        (keep-high-confidence-level
         (read-in-segements "line-segments.csv")))))