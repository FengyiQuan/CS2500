;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname interview_code) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
#|INSTRUCTIONS:
- Please leave feedback as you would if you were a tutor/grader.
- You may assume that everything above the dashed line was given so there is no need to leave
  feedback on that portion of the code.
- You may assume that students have not yet learned list abstractions when completing this
  assignment. Based on the syllabus for spring 2019 this would mean that the date is somewhere
  between February 11 and February 27.|#

(define-struct course [name sections prereqs])
;; A Course is a (make-course String [List-of Section] [List-of Course])
;; - name is the course name
;; - sections is a list of the sections being offered
;; - and prereqs is a list of course pre-requisites

(define-struct online [id prof])
(define-struct section [id prof when where])
;; A Section is one of:
;; - (make-online Nat String)
;;   - id is the unique identifier for this section (the CRN)
;;   - prof is the name of the professor teaching this section
;; - (make-section Nat String String String)
;;   - id is the unique identifier for this section (the CRN)
;;   - prof is the name of the professor teaching this section
;;   - when is the time that this section is taught
;;   - and where is the location where this section is taught

;; A Semester is a [List-of Course]

;; Design the function find-all-profs which, given a Semester, returns a list of all the unique
;; names of professors who are teaching that semester. The registrar is not very organized so some
;; courses may only be listed as pre-requisites for other courses.

#|------------------------------------------------------------------------------------------------|#
(define SECTION1 (make-section 41324 "Elena Strange" "11:40-1:20" "SN005"))
(define DISCRETE (make-course "CS1800" (list SECTION1) '()))

;; find-all-profs : Semester -> ListOfString
; no purpose statement for find-all-profs
; not enough tests: test more cases when there are multiple elements in the list
; need to check if list of string has the duplicate name of profs, should delete the duplicate ones
(check-expect (find-all-profs '()) '())
(check-expect (find-all-profs (list DISCRETE)) '(("Elena Strange")))
(define (find-all-profs s)
  (cond [(empty? s) '()]
        [(cons? s)
         ; cannot cons a list of string and a list of string 
         (cons (helper-for-find-profs (first s))
               (find-all-profs (rest s)))]))

;; helper-for-find-profs : Course -> String
;; Help to find the names of the profs
; incorrect signature, should be Course -> ListOfString
(check-expect (helper-for-find-profs DISCRETE) (list "Elena Strange"))
(check-expect (helper-for-find-profs (make-course "Nothing" '() '())) '())
(define (helper-for-find-profs c)
  (cond [(empty? (course-sections c)) (helper-for-reqs c)]
        [(cons? (course-sections c))
         (cons (section-prof (first (course-sections c)))
               ; not follow template
               (helper-for-find-profs
                ; it is not very reasonable for making a new course, or it should be a helper to
                ; remove the first section in a list
                (make-course (course-name c)
                             (rest (course-sections c))
                             (course-prereqs c))))]))

; function name is not very descriptive
;; helper-for-reqs : Course -> Los
; Los should not been shorten, should be ListOfString 
;; Takes a course and produces a list of professor names
(check-expect (helper-for-reqs DISCRETE) '())
(define (helper-for-reqs c)
  (cond [(empty? (course-prereqs c)) '()]
        [(cons? (course-prereqs c))
         ; not follow template
         (cons (helper-for-find-profs (first (course-prereqs c)))
               (helper-for-reqs
                ; same as above, not very reasonable
                (make-course (course-name c) (course-sections c)
                             (rest (course-prereqs c)))))]))

;; it is not a good way to define this function by creating a new Course by remoeing the first item
;; every time; follow template should give your a better understanding of this problem
;; you probably need to define some helper function when a function doing more than one job