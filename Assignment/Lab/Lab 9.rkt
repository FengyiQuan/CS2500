;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require "files.rkt")
(define EMPTY-DIR (make-dir "hopes&dreams" '() '()))
(define CAREER
  (make-dir
   "Career"
   (list (make-dir "CareerApplications"
                   '()
                   (list (make-file "CoverLetter.doc" 31744
                                    (make-date 2015 9 20 11 36 25) "")
                         (make-file "EmploymentApplication.pdf" 231010
                                    (make-date 2015 10 13 13 10 0) "")))
         (make-dir "CareerMyJob"
                   '()
                   (list (make-file "BackgroundCheck.pdf" 1040138
                                    (make-date 2016 8 23 10 27 10) "")
                         (make-file "I9.pdf" 963654
                                    (make-date 2015 11 20 15 49 45) "")
                         (make-file "JobOffer.pdf" 507887
                                    (make-date 2015 11 20 15 49 0) ""))))
   (list (make-file "References.docx" 11634
                    (make-date 2016 8 6 9 55 15) "")
         (make-file "Resume.doc" 34816
                    (make-date 2016 10 12 13 18 12) "")
         (make-file "Transcript.doc" 140288
                    (make-date 2015 9 11 9 3 0) ""))))
 
; file-temp : File -> ?
(define (file-temp f)
  (... (file-name f) (file-size f) (date-temp (file-date f)) (file-content f)))
 
; date-temp : Date -> ?
(define (date-temp d)
  (... (date-year d) (date-month d) (date-hours d) (date-minutes d) (date-seconds d)))
 
; directory-temp : Directory -> ?
(define (directory-temp d)
  (... (dir-name d) (lod-temp (dir-dirs d)) (lof-temp (dir-files d))))
 
; lod-temp : [List-of Directory] -> ?
(define (lod-temp lod)
  (cond [(empty? lod) ...]
        [(cons? lod) (... (directory-temp (first lod)) (lod-temp (rest lod)))]))
 
; lof-temp : [List-of File] -> ?
(define (lof-temp lof)
  (cond [(empty? lof) ...]
        [(cons? lof) (... (file-temp (first lof)) (lof-temp (rest lof)))]))

;; Exercise (Reviewed) 1
;; count-files : Directory -> Number
;; consumes a Directory and produces the total number of files in it
(check-expect (count-files EMPTY-DIR) 0)
(check-expect (count-files CAREER) 8)
(define (count-files dir)
  (local [;  Directory Number -> Number
          ; counts the subdirectories within a dir
          (define (count-each-subdir subd num)
            (+ (count-files subd) num))]
    (+ (foldr count-each-subdir 0 (dir-dirs dir))
       (length (dir-files dir)))))

;; Exercise 2
(define DIR-0 (get-dir "123"))

;; Exercise 3
;; same-name? : Directory String -> Boolean
;; determines whether or not a file with that name
;; exists in the directory or any of its subdirectories
(check-expect (same-name? DIR-0 "123") #f)
(check-expect (same-name? DIR-0 "Lab 1.rkt") #t)
(define (same-name? d s)
  (local [;; rest-dir-same-name?
          (define (rest-dir-same-name? lod)
            (cond [(empty? lod) #f]
                  [(cons? lod) (or (same-name? (first lod) s)
                                   (rest-dir-same-name? (rest lod)))]))
          ;; any-same-name-in-lof?
          (define (any-same-name-in-lof? lof)
            (cond [(empty? lof) #f]
                  [(cons? lof) (or (string=? (file-name (first lof)) s)
                                   (any-same-name-in-lof? (rest lof)))]))]
    (or (rest-dir-same-name? (dir-dirs d))
        (any-same-name-in-lof? (dir-files d)))))

;; Exercise 4
;; size-file : Directory -> Number
;; consumes a Directory and computes the total size of all files in the whole directory tree
(check-expect (size-file DIR-0) 14951)
(define (size-file d)
    (+ (foldr + 0 (map size-file (dir-dirs d)))
       (foldr + 0 (map file-size (dir-files d)))))
  