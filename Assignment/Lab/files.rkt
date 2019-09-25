#lang racket
(require htdp/dir htdp/error)

(define (dir-symbols->strings d)
  (make-dir (symbol->string (dir-name d)) (map dir-symbols->strings (dir-dirs d)) (dir-files d)))

(define (get-dir a-path)
  (check-arg 'get-dir (string? a-path) "string" "first" a-path)
  (let ([a-path! (string->path a-path)])
    (if (directory-exists? a-path!)
        (dir-symbols->strings (create-dir a-path))
        (error 'get-dir "not a directory: ~e" a-path))))

(provide
 
 make-file
 file?
 file-name
 file-size
 file-date
 file-content
 
 make-date
 date?
 date-year
 date-month
 date-day
 date-hours
 date-minutes
 date-seconds
 
 make-dir
 dir?
 dir-name
 dir-dirs
 dir-files
 
 get-dir)