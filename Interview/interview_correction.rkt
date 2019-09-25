;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname interview_correction) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

;; find-all-profs : Semester -> [List-of String]
(define (find-all-profs s)
  (remove-duplicate (get-all-name-duplicate s)))

;; remove-duplicate : [List-of String] -> [List-of String]
;; remove duplicate name in a list of string
(define (remove-duplicate lon)
  (cond [(empty? lon) lon]
        [(cons? lon) (if (any-same-name? (first lon)(rest lon))
                         (remove-duplicate (rest lon))
                         (cons (first lon) (remove-duplicate (rest lon))))]))

;; any-same-name? : String ListOfString -> Boolean
;; check if any same name in a list as given string
(define (any-same-name? name los)
  (cond [(empty? los) #false]
        [(cons? los)(or (string=? (first los) name)
                        (any-same-name? name (rest los)))]))

;; get-all-name-duplicate : Semester -> [List-of String]
;; get all name by given a semester
(define (get-all-name-duplicate loc)
  (cond
    [(empty? loc) (list)]
    [(cons? loc) (append (get-all-name-by-course (first loc))
                         (get-all-name-duplicate (rest loc)))]))

;; get-all-name-by-course : Course -> [List-of String]
;; get all profs name by giben a course
(define (get-all-name-by-course c)
  (append (get-all-name-by-sections (course-sections c))
          (get-all-name-duplicate (course-prereqs c))))

;; get-all-name-by-sections : [List-of Section] -> [List-of String]
(define (get-all-name-by-sections los)
  (cond
    [(empty? los) (list)]
    [(cons? los) (cons (get-name-by-one-section (first los))
                       (get-all-name-by-sections (rest los)))]))

;; get-name-by-one-section : Section -> String
(define (get-name-by-one-section s)
  (cond
    [(online? s) (online-prof s)]
    [(section? s) (section-prof s)]))


(define SECTION1 (make-section 41324 "Elena Strange" "11:40-1:20" "SN005"))
(define DISCRETE (make-course "CS1800" (list SECTION1) '()))

(define section1 (make-online 1 "nate"))
(define section2 (make-section 1 "nate" "1" "1"))
(define section3 (make-section 1 "ravi" "1" "1"))
(define section4 (make-section 1 "leena" "1" "1"))
(define section5 (make-section 1 "nate" "1" "1"))
(define cs2500 (make-course "cs2500" (list section1 section2) '()))
(define cs1800 (make-course "cs2500" (list section3) '() ))
(define cs2510 (make-course  "cs2500" (list section4 section5) (list cs2500 cs1800)))
