;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 12|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)
;; Exercise 1
;; remove-punctuation : String -> String
;; accepts a String and removes 1 instance of ",", ";", or "."
;; from the end the input String if it is there.
(check-expect (remove-punctuation "When the last days were upon me,")
              "When the last days were upon me")
(check-expect (remove-punctuation "I,.") "I,")
(check-expect (remove-punctuation ",1,;") ",1,")
(check-expect (remove-punctuation "") "")
(define (remove-punctuation s)
  (cond
    [(string=? s "") ""]
    [(or (string=? (string-ith s (- (string-length s) 1)) ",")
         (string=? (string-ith s (- (string-length s) 1)) ".")
         (string=? (string-ith s (- (string-length s) 1)) ";"))
     (substring s 0 (- (string-length s) 1))]
    [else s]))

;; Exercise 2
;; remove-downcase : String -> String
;; first remove-punctuation from a String and then convert it to its lowercase variant
(check-expect (remove-downcase "") "")
(check-expect (remove-downcase "ASDFV") "asdfv")
(check-expect (remove-downcase "When the last days were upon me,") "when the last days were upon me")
(define remove-downcase
  (compose remove-punctuation string-downcase))

;; Exercise 3
;; compound-word? : String -> Boolean
;; determines if the given String contains "-"
(check-expect (compound-word? "") #false)
(check-expect (compound-word? "a") #false)
(check-expect (compound-word? "a-sd") #true)
(define (compound-word? s)
  (string-contains? "-" s))

; An [NEList-of X] (Non-Empty List of X) is one of:
; - (cons X '())
; - (cons X [NEList-of X])
 
; bin : [X X -> Boolean] [List-of X] -> [List-of [NEList-of X]]
; Bin lox by matches?
(check-expect (bin = '()) '())
(check-expect (bin = (list 2 3 4 2)) (list (list 2 2) (list 4) (list 3)))
(define (bin matches? lox)
  (local [; find-spot-for-x : X [List-of [NEList-of X]] -> [List-of [NEList-of X]]
          ; Find the spot for x
          (define (find-spot-for-x x bins)
            (find-spot x matches? bins))]
    (foldr find-spot-for-x '() lox)))
 
; find-spot : X [X X -> Boolean] [List-of [NEList-of X]] -> [List-of [NEList-of X]]
; Find where x belongs and place it in the appropriate list (or give it its own)
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
;; produces the unique compound words in the Strings
(check-expect (unique-compound-words '()) '())
(check-expect (unique-compound-words (list "a" "A" "b-" "b")) (list "b-"))
(check-expect (unique-compound-words (list "" "")) '())
(check-expect (unique-compound-words (list "a" "B-" "asd-" "")) (list "b-" "asd-"))
(define (unique-compound-words los)
  (foldl only-unique '() (cleaned-and-binned los)))

;; only-unique : [List-of String] [List-of [List-of String]] -> [List-of [List-of String]]
;; only keeps the list whose size is less or equal than 1, and removes elements
;; that appear more than once
(check-expect (only-unique '() (list '())) (list '()))
(check-expect (only-unique (list "b" "A")(list (list "b-") (list "a-" "a-")))
              (list (list "b-") (list "a-" "a-")))
(check-expect (only-unique (list "A")(list (list "b-") (list "a-" "a-")))
              (list "A" (list "b-") (list "a-" "a-")))
(define (only-unique los lolos)
  (if (<= (length los) 1)
      (append los lolos)
      lolos))

;; cleaned-and-binned : [List-of String] -> [List-of [List-of String]]
;; filters out the non-compound string and low-cases and removes punctuation from Strings,
;; and bins the same elements together
(check-expect (cleaned-and-binned '()) '())
(check-expect (cleaned-and-binned (list "a" "a" "ab" "bc" "c")) '())
(check-expect (cleaned-and-binned (list "a-" "a-" "ab" "bc" "c")) (list (list "a-" "a-")))
(check-expect (cleaned-and-binned (list "" "")) '())
(check-expect (cleaned-and-binned (list "A-" "a-" "b-,")) (list (list "b-") (list "a-" "a-")))
(define (cleaned-and-binned los)
  (bin string=? (map remove-downcase (filter compound-word? los))))

;; costant:
(define UNIQUE-COMPOUND-WORDS (unique-compound-words (read-words "exoblivione.txt")))

; A RawInput is a (list String String String String String String String)
; and represents the x1, y1, z1, x2, y2, z2, and confidence level of a line in 3d space
 
; A NumericInput is a (list Number Number Number Number Number Number [0, 1])
; and represents the x1, y1, z1, x2, y2, z2, and confidence level in the accuracy of the data
 
; raw->numeric : RawInput -> NumericInput
; Convert raw input to numeric input
(check-expect (raw->numeric (list "0" "1" "2" "3" "4" "5" ".4")) (list 0 1 2 3 4 5 0.4))
(define (raw->numeric raw)
  (map string->number raw))
 
; read-in-segements : String -> [List-of NumericInput]
; Read in the line segments contained in s and map to numeric input
(define (read-in-segements s)
  (read-csv-file/rows s raw->numeric))

;; Exercise 5
(define NUMERICINPUT-1 (list 0 1 2 3 4 5 0.4))
(define NUMERICINPUT-2 (list 0 1 2 3 4 5 0.1))
(define NUMERICINPUT-3 (list 0 1 2 3 4 5 0.2))
;; filter-num : [List-of NumericInput] -> [List-of NumericInput]
;; filters a [List-of NumericInput] and returns only those which
;; have a confidence level of 0.2 or higher
(check-expect (filters (list NUMERICINPUT-1 NUMERICINPUT-2)) (list NUMERICINPUT-1))
(check-expect (filters (list NUMERICINPUT-2)) '())
(define (filters lonumeric)
  (filter >0.2? lonumeric))

;; >0.2? : NumericInput -> Boolean
;; determine if confidence level of NumericInput is 0.2 or higher
(check-expect (>0.2? NUMERICINPUT-1) #true)
(check-expect (>0.2? NUMERICINPUT-2) #false)
(check-expect (>0.2? NUMERICINPUT-3) #true)
(define (>0.2? numeric)
  (>= (seventh numeric) 0.2))

;; Exercise 6
(define-struct 3d-point [x y z])
;; A 3d-Point is a (make-3d-point Number Number Number)
;; where x is x-coordinate of point
;; where y is y-coordinate of point
;; where z is z-coordinate of point
;; Examples:
(define 3DP-1 (make-3d-point 1 1 1))
(define 3DP-2 (make-3d-point 1 2 1))
(define 3DP-3 (make-3d-point 1 1 4))
(define 3DP-4 (make-3d-point 2 1 4))
#;
(define (3d-point-temp 3dp)
  (... (3d-point-x 3dp) ...
       (3d-point-y 3dp) ...
       (3d-point-z 3dp) ...))

(define-struct linesegment [p1 p2])
;; A LineSegment is a (make-linesegment 3d-Point 3d-Point)
;; where p1 is one point (start point) in 3d space
;; where p2 is another point (end point) in 3d space
;; Examples:
(define LS-1 (make-linesegment 3DP-1 3DP-2))
(define LS-2 (make-linesegment 3DP-1 3DP-3))
(define LS-3 (make-linesegment 3DP-2 3DP-3))
(define LS-4 (make-linesegment 3DP-2 3DP-4))
#;
(define (linesegment-temp l)
  (... (3d-point-temp (linesegment-p1 l)) ...
       (3d-point-temp (linesegment-p2 l)) ...))

;; Exercise 7
;; numeric->linesegment : NumericInput -> LineSegment
;; converts a NumericInput into a LineSegment
(check-expect (numeric->linesegment (list 0 1 2 3 4 5 0.4)) (make-linesegment (make-3d-point 0 1 2)
                                                                              (make-3d-point 3 4 5)))
(define (numeric->linesegment nu)
  (make-linesegment (make-3d-point (first nu) (second nu) (third nu))
                    (make-3d-point (fourth nu) (fifth nu) (sixth nu))))

;; Exercise 8
;; flips : LineSegment -> LineSegment
;; "flips" a LineSegment by swapping its two points
(check-expect (flips LS-1) (make-linesegment 3DP-2 3DP-1))
(define (flips ls)
  (make-linesegment (linesegment-p2 ls) (linesegment-p1 ls)))

;; Exercise 9
;; attached-at-first-point? : LineSegment LineSegment -> Boolean
;; accepts two LineSegments and determines if they have the same first point
(check-expect (attached-at-first-point? LS-1 LS-2) #true)
(check-expect (attached-at-first-point? LS-1 LS-1) #true)
(check-expect (attached-at-first-point? LS-1 LS-3) #false)
(check-expect (attached-at-first-point? LS-2 LS-3) #false)
(define (attached-at-first-point? sg1 sg2)
  (same-point? (linesegment-p1 sg1) (linesegment-p1 sg2)))

;; same-point? : 3d-Point 3d-Point -> Boolean
;; determine if two 3d-Point are the same
(check-expect (same-point? 3DP-1 3DP-1) #true)
(check-expect (same-point? 3DP-2 3DP-1) #false)
(define (same-point? p1 p2)
  (and (= (3d-point-x p1) (3d-point-x p2))
       (= (3d-point-y p1) (3d-point-y p2))
       (= (3d-point-z p1) (3d-point-z p2))))

;; Exercise 10
;; bin-ls : [List-of LineSegment] -> [List-of [List-of LineSegments]]
;; accepts a [List-of LineSegment] and bins them by their first point
(check-expect (bin-ls '()) '())
(check-expect (bin-ls (list LS-1 LS-2)) (list
                                         (list (make-linesegment 3DP-3 3DP-1))
                                         (list (make-linesegment 3DP-2 3DP-1))
                                         (list LS-1 LS-2)))
(check-expect (bin-ls (list LS-1 LS-2 LS-3)) (list
                                              (list (make-linesegment 3DP-3 3DP-1)
                                                    (make-linesegment 3DP-3 3DP-2))
                                              (list LS-3 (make-linesegment 3DP-2 3DP-1))
                                              (list LS-1 LS-2)))
                     
(define (bin-ls lols)
  (bin attached-at-first-point? (combined-list lols)))

;; combined-list : [List-of LineSegment] -> [List-of LineSegment]
;; builds a new list that contains both original list of LineSegment and its flipped LineSegment
(check-expect (combined-list (list LS-1 LS-2)) (list LS-1 LS-2
                                                     (make-linesegment 3DP-2 3DP-1)
                                                     (make-linesegment 3DP-3 3DP-1)))
(define (combined-list lols)
  (append lols (map flips lols)))

;; Exercise 11
;; pairs-number : [List-of X] -> Number
;; given a [List-of X], computes the number of pairs of elements
(check-expect (pairs-number '()) 0)
(check-expect (pairs-number (list 1)) 0)
(check-expect (pairs-number (list 1 2)) 1)
(check-expect (pairs-number (list 1 2 3 4 5)) 10)
(check-expect (pairs-number (list "" "a" "sdf" "asd" "qwe")) 10)
(define (pairs-number lox)
  (/ (* (length lox) (- (length lox) 1)) 2))

;; Exercise 12
; pair-count : [List-of LineSegment] -> Number
; bins a list of LineSegment and counts the pairs in all bins
(check-expect (pair-count '()) 0)
(check-expect (pair-count (list LS-1 LS-2 LS-3)) 3)
(define (pair-count lols)
  (foldr count 0 (bin-ls lols)))

;; count : [List-of LineSegment] Number -> Number
;; counts the pairs in a list of list of LineSegment
(check-expect (count '() 0) 0)
(check-expect (count (list LS-1 LS-2) 0) 1)
(check-expect (count (list LS-1 LS-2 LS-3) 0) 3)
(define (count bin total)
  (+ (pairs-number bin) total))

;; Exercise 13
(define NUMBER-OF-PAIRS (pair-count 
                         (map numeric->linesegment
                              (filters (read-in-segements "line-segments.csv")))))