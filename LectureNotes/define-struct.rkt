;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname define-struct) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A MoonSunPosition is (make-posn (make-posn num num) (make-posn num num))
; V: define-struct
; G: (define-struct structname [field1 field2])
; S: structname is now a data type

(define-struct moonsunpos [moonx moony sunx suny])

; Constructor: (make-structname v1 v2 v3 ...)
; Selectors: (structname-field1 (make-structname))
; Predicate: (structname? Any) -> Boolean

; Design a function named salutation that takes in a student
; and outputs "Dear FIRSTNAME LASTNAME (NUID):"

(define-struct STUDENT [FIRSTNAME LASTNAME NUID])

; A Student is )make-studnet STRING STRING Number)
; Interpretation:
; fname is the student's first name
; lname is the student's last name
; nuid is the student's Northeastern student id
; Examples:
(define STUDENT-1 (make-STUDENT "john" "doe" 12345))
(define STUDENT-2 (make-STUDENT "jenny" "jane" 8675309))

#;
(define (STUDENT-temp s)
  (... (STUDENT-FIRSTNAME s) ...(STUDENT-LASTNAME s)...(STUDENT-NUID s) ... ))

; salutation : Student -> String
; produces a greeting for a student by name/nuid
(check-expect (salutation STUDENT-1)
              "Dear john doe (12345)")
(check-expect (salutation STUDENT-2)
              "Dear jenny jane (8675309)")

(define (salutation STUDENT)
  (string-append
   "Dear "
   (STUDENT-FIRSTNAME STUDENT) " "
   (STUDENT-LASTNAME STUDENT) " ("
   (number->string (STUDENT-NUID STUDENT)) ")" ))

