;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname temolace) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;A NumericGrade is a real number in [0, 100]
;Interpretation: a student's grade in a class
;Examples:
(define NUMERIC-GRADE-65 65)
(define NUMERIC-GRADE-5 5)
(define NUMERIC-GRADE-98.5 98.5)

;(define (numeric-grade-temp ng)
  



; A LetterGrade is one of:
; - "A"
; - "B"
; - "C"
; - "D"
; - "E"
; - "F"
; Interpretation: a student's letter grde in the US system
;Examples:
(define LETTER-GRADE-A "A")
(define LETTER-GRADE-B "B")
(define LETTER-GRADE-C "C")
(define LETTER-GRADE-D "D")
(define LETTER-GRADE-E "E")
(define LETTER-GRADE-F "F")

(define (letter-grade-temp lg)
  (cond
    [(string=? lg LETTER-GRADE-A) ...]
    [(string=? lg LETTER-GRADE-A) ...]
    [(string=? lg LETTER-GRADE-A) ...]
    [(string=? lg LETTER-GRADE-A) ...]
    [(string=? lg LETTER-GRADE-A) ...]))


; A GPA is real number in [0, 4]
; INterpretation: a GPA in NEU
; Examples:
(define GPA-2.2 2.2)
(define GPA-4 4.0)
(define GPA-2 2.0)

;(define (gpa-temp gpa))

; grade->GPA : LetterGrade -> GPA
; compute the GPA for a letter grade

(check-expect (grade->GPA LETTER-GRADE-A) GPA-4)
(check-expect (grade->GPA LETTER-GRADE-C) GPA-2)

(define (grade->GPA grade)
  (cond
    [(string=? grade LETTER-GRADE-A) 4.0]
    [(string=? grade LETTER-GRADE-A) 3.0]
    [(string=? grade LETTER-GRADE-A) 2.0]
    [(string=? grade LETTER-GRADE-A) 1.0]
    [(string=? grade LETTER-GRADE-A) 0.0]))

