;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname bigger-than?) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; Design a function which determines if every number
;; in a list is bigger than a given number

;; bigger-than-n? : [List-of Number] Number -> Boolean
(check-expect (bigger-than-n? (list) 3.14159) #t)
(check-expect (bigger-than-n? (list 1 2 3) 3.14159) #f)
(check-expect (bigger-than-n? (list 5 6 7) 3.14159) #t)
(check-expect (bigger-than-n? (list 3.14159) 3.14159) #f)

(define (bigger-than-n? lon n)
  (local [;; bigger-than? : Number -> Boolean
          (define (bigger-than? num)
            (> num n))]
    (andmap bigger-than? lon)))

;; Design a function which negtes a predicate

;; negate : (X) [X -> Boolean] -> [X -> Boolean]


(define (negate.v1 p?)
  (local [;; foo : X -> Boolean
          (define (foo x)
            (not (p? x)))]
    foo))

(define (negate p?)
  (compose not p?))

(define non-positive? (negate positive?) )
(check-expect (non-positive? 5) #f)
(check-expect (non-positive? -1) #t)

(define not-string? (negate string?))
(check-expect (not-string? "a") #f)
(check-expect (not-string? 3.1234) #t)

;;
;; A BasePair is one of:
;; - "G"
;; - "A"
;; - "T"
;; - "C"
;; A single base pair in DNA

;; A Sequence is a [List-of BasePair]

;; Design the function num-matches that accepts two Sequences
;; and returns the number of places where they contain the same base pair

; num-matches : Sequence Sequence -> Number
(check-expect (num-matches '() '()) 0)
(check-expect (num-matches (explode "ACT") (explode "TCA")) 1)
(check-expect (num-matches (explode "ACT") (explode "ACT")) 3)

(define (num-matches seq1 seq2)
  (cond
    [(or (empty? seq1) (empty? seq2)) 0]
    [(and (cons? seq1) (cons? seq2)) (if (string=? (first seq1) (first seq2))
                                         (add1 (num-matches (rest seq1) (rest seq2)))
                                         (num-matches (rest seq1) (rest seq2)))]))


;; Without using append. define the function append-sequences
;; that accepts two Sequences and computes the sequence of the
;; first followed by the second

; append-sequences : Sequence Sequence -> Sequence
(check-expect (append-sequences (explode "ACT") (explode "GGG"))
              (explode "ACTGGG"))
(define (append-sequences.v1 seq1 seq2)
  (cond
    [(empty? seq1) seq2]
    [(cons? seq2) (cons (first seq1)
                        (append-sequences (rest seq1) seq2))]))

(define (append-sequences seq1 seq2)
  (foldr cons seq2 seq1))


;; Courses at Northeastern have a subject, a number, a title, and a set of prerequisites

(define-struct course [subject number title prereqs])
;; A Course is (make-course String Number String [List-of Course])

(define COURSE-1 (make-course "CS" 2500 "Fundies I" '()))
(define COURSE-2 (make-course "CS" 2510 "Fundies II" (list COURSE-1)))
(define COURSE-3 (make-course "CS" 3200 "Databases" (list COURSE-2)))
(define COURSE-4 (make-course "CS" 2510 "OOD" (list COURSE-2)))
(define COURSE-5 (make-course "CS" 4500 "Software Development" (list COURSE-3 COURSE-4)))

#;(define (course-temp c)
    ... (course-subject c) ...
    ... (course-number c) ...
    ... (course-title c) ...
    ... (loc-temp (course-prereqs c)) ...)


































