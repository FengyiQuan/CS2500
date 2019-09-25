;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample10) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct book [title author page-count])
(define-struct article [title author url word-count])
; An InfoSource is one of:
; - (make-book String String Number)
; - (make-article String String String Number)
; and represents either:
; - a book's name, author, and number of pages
; - an online article's name, author, url, and number of words
 
(define INFO-SOURCE/BOOK (make-book "On Directing Film" "David Mamet" 128))
(define INFO-SOURCE/ARTICLE
  (make-article "How Did Moonlight Win Best Picture?"
                "Kyle Buchanan"
                "http://www.vulture.com/2017/02/oscars-2017-how-did-moonlight-win-best-picture.html"
                1285))

(define LOI (list INFO-SOURCE/BOOK INFO-SOURCE/ARTICLE))
 
; infosource-temp : InfoSource -> ?
(define (infosource-temp i)
  (cond [(book? i) (... (book-title i) (book-author i) (book-page-count i))]
        [(article? i) (... (article-title i)
                           (article-author i)
                           (article-url i)
                           (article-word-count i))]))

;; Exercise 1

;; only-books : [List-of InfoSource] -> [List-of InfoSource]
;; Only the books in loi
(check-expect (only-books LOI) (list INFO-SOURCE/BOOK))
(define (only-books loi)
  (filter book? loi))

;; Exercise 2

;; titles : [List-of InfoSource] -> [List-of String]
;; The titles of the info sources
(check-expect (titles LOI)
              '("On Directing Film" "How Did Moonlight Win Best Picture?"))
(define (titles loi)
  (map info-title loi))

;; info-title : InfoSource -> String
;; The title of the info source
(check-expect (info-title INFO-SOURCE/BOOK) "On Directing Film")
(check-expect (info-title INFO-SOURCE/ARTICLE) "How Did Moonlight Win Best Picture?")
(define (info-title i)
  (cond [(book? i) (book-title i)]
        [(article? i) (article-title i)]))

;; Exercise 3

;; authors : [List-of InfoSource] -> String
;; The authors seperated by a space
(check-expect (authors '()) "")
(check-expect (authors LOI)
              "David Mamet Kyle Buchanan ")
(define (authors loi)
  (fold-info loi put-space-between info-author "")
  #;(cond [(empty? loi) ""]
          [(cons? loi) (string-append (info-author (first loi)) " " (authors (rest loi)))]))

;; info-author : InfoSource -> String
;; Author of info source
(check-expect (info-author INFO-SOURCE/BOOK) "David Mamet")
(check-expect (info-author INFO-SOURCE/ARTICLE) "Kyle Buchanan")
(define (info-author i)
  (cond [(book? i) (book-author i)]
        [(article? i) (article-author i)]))

;; Exercise 4

;; total-pages : [List-of InfoSource] -> Number
;; Total pages of the info sources
(check-expect (total-pages '()) 0)
(check-expect (total-pages LOI) 141)
(define (total-pages loi)
  (fold-info loi + info-pages 0)
  #;(cond [(empty? loi) 0]
          [(cons? loi) (+ (info-pages (first loi)) (total-pages (rest loi)))]))

;; info-pages : InfoSource -> Number
;; Total pages in info source (assume 100 words/page)
(check-expect (info-pages INFO-SOURCE/BOOK) 128)
(check-expect (info-pages INFO-SOURCE/ARTICLE) 13)
(define (info-pages i)
  (cond [(book? i) (book-page-count i)]
        [(article? i) (ceiling (/ (article-word-count i) 100))]))

;; Exercise 5

;; fold-info: [List-of InfoSource] [X X -> X] [InfoSource -> X] X -> X
;; Fold over the info sources
(define (fold-info loi combine convert base)
  (cond [(empty? loi) base]
        [(cons? loi) (combine (convert (first loi))
                              (fold-info (rest loi) combine convert base))]))

;; put-space-between : String String -> String
;; Put space between the two spaces
(check-expect (put-space-between "cat" "dog") "cat dog")
(define (put-space-between s1 s2)
  (string-append s1 " " s2))

;; fold-info/alt :[List-of InfoSource] [InfoSource X -> X] X -> X
;; Fold over the info sources
(define (fold-info/alt loi combine base)
    (cond [(empty? loi) base]
          [(cons? loi) (combine (first loi)
                                (fold-info/alt (rest loi) combine base))]))