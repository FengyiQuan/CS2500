;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 10|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct book [title author page-count])
(define-struct article [title author url word-count])
; An InfoSource is one of:
; - (make-book String String Number)
; - (make-article String String String Number)
; and represents either:
; - a book's name, author, and number of pages
; - an online article's name, author, url, and number of words
;; Examples:
(define INFO-SOURCE/BOOK (make-book "On Directing Film" "David Mamet" 128))
(define INFO-SOURCE/ARTICLE
  (make-article "How Did Moonlight Win Best Picture?"
                "Kyle Buchanan"
                "http://www.vulture.com/2017/02/oscars-2017-how-did-moonlight-win-best-picture.html"
                1285))
 
; infosource-temp : InfoSource -> ?
(define (infosource-temp i)
  (cond [(book? i) (... (book-title i) (book-author i) (book-page-count i))]
        [(article? i) (... (article-title i)
                           (article-author i)
                           (article-url i)
                           (article-word-count i))]))
;; Exercise 1
;; only-books : [List-of InfoSource] -> [Lisr-of Books]
;; accepts a list of info sources, and produces a list of only the books
(check-expect (only-books (list INFO-SOURCE/BOOK INFO-SOURCE/ARTICLE)) (list INFO-SOURCE/BOOK))
(define (only-books loi)
  (filter book? loi))

;; Exercise 2
;; list-title : [List-of InfoSource] -> [List-of String]
;; returns a list of the titles of every info source in a list of info sources
(check-expect (list-title (list INFO-SOURCE/BOOK INFO-SOURCE/ARTICLE))
              (list "On Directing Film" "How Did Moonlight Win Best Picture?"))
(define (list-title loi)
  (local [;; abstract-title : InfoSource -> String
          ;; takes a InfoSource and returns the title of this InfoSource
          (define (abstract-title ifs)
            (cond
              [(book? ifs) (book-title ifs)]
              [(article? ifs) (article-title ifs)]))]
    (map abstract-title loi)))

;; Exercise 3
;; author-string : [List-of InfoSource] -> String
;; returns a String of all of the authors in a list of info sources, separated by " "
(check-expect (author-string (list INFO-SOURCE/BOOK INFO-SOURCE/ARTICLE))
              "David Mamet Kyle Buchanan ")
(define (author-string loi)
  (cond
    [(empty? loi) ""]
    [(cons? loi) (string-append (author-of-ifs (first loi))
                                (author-string (rest loi)))]))

;; author-of-ifs : InfoSource -> String
;; returns the name of authors of a InfoSource
(check-expect (author-of-ifs INFO-SOURCE/BOOK) "David Mamet ")
(check-expect (author-of-ifs INFO-SOURCE/ARTICLE) "Kyle Buchanan ")
(define (author-of-ifs i)
  (cond [(book? i) (string-append (book-author i) " ")]
        [(article? i) (string-append (article-author i) " ")]))

;; Exercise 4
;; total-page-count : [List-of InfoSource] -> Number
;; returns the total page count of the pages in a list of info sources
(check-expect (total-page-count (list INFO-SOURCE/BOOK INFO-SOURCE/ARTICLE)) 141)
(define (total-page-count loi)
  (cond
    [(empty? loi) 0]
    [(cons? loi) (+ (page-number-of-ifs (first loi))
                    (total-page-count (rest loi)))]))

;; page-number-of-ifs : InfoSource -> Number
;; calculates the total page number of infoSource
(check-expect (page-number-of-ifs INFO-SOURCE/BOOK) 128)
(check-expect (page-number-of-ifs INFO-SOURCE/ARTICLE) 13)
(define (page-number-of-ifs i)
  (cond [(book? i) (book-page-count i)]
        [(article? i) (ceiling (/ (article-word-count i)
                                  100))]))

;; Exercise 5
;; abstraction : (X) [List-of InfoSource] X [X X -> X] [InfoSource -> X] -> X
(define (abstraction loi base func-to-all func-to-first)
  (cond
    [(empty? loi) base]
    [(cons? loi) (func-to-all (func-to-first (first loi))
                              (abstraction (rest loi) base func-to-all func-to-first))]))

;; author-string : [List-of InfoSource] -> String
;; returns a String of all of the authors in a list of info sources, separated by " "
(define (author-string.2 loi)
  (abstraction loi "" string-append author-of-ifs))

;; total-page-count : [List-of InfoSource] -> Number
;; returns the total page count of the pages in a list of info sources
(define (total-page-count.2 loi)
  (abstraction loi 0 + page-number-of-ifs))