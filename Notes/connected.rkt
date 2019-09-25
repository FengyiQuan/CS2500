;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname connected) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp")) #f)))
(define-struct page [title links])

; A Page is a (make-page String [List-of String])
; Interpretation: web page's title and links to other pages.
; Examples ...
#;
(define (page-temp wp)
  ... (page-title wp) ... (los-temp (page-links wp)))

(define PAGE-0 (make-page "CS 2500"    (list "Thomas")))
(define PAGE-1 (make-page "CS Classes" (list "CS 2500")))
(define PAGE-2 (make-page "Thomas"     (list "CS 2500" "CS Classes")))
(define PAGE-3 (make-page "Faculty"    (list "Thomas")))

; A Wiki is a [List-of Page]
; Interpretation: A list of pages in a wiki
(define WIKI-0 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3))
#;
(define (wiki-temp w)
  (cond
    [(empty? w) ...]
    [(cons?  w) ... (page-temp (first w)) ... (wiki-temp (rest w)) ...]))


; Design a function that returns the number of pages directly mentioned
; in the pages of a given wiki.

; num-pages : Wiki -> Nat
; Returns the number of pages directly mentioned in a wiki
(check-expect (num-pages (list PAGE-0)) 2)
(check-expect (num-pages WIKI-0)        4)
(define (num-pages w)
  (length (get-pages w)))

; get-pages : Wiki -> [List-of String]
; return all pages mentioned in the Wiki
(define (get-pages w)
  (cond
    [(empty? w) '()]
    [(cons?  w) (merge (cons (page-title (first w)) (page-links (first w))) (get-pages (rest w)))]))

; merge : [List-of String] [List-of String] -> [List-of String]
; merge the two lists, i.e. avoid duplicates
(check-expect (merge (list "A" "B") (list "A" "C")) (list "B" "A" "C")) ; !!!
(define (merge l1 l2)
  (cond [(empty? l1) l2]
        [(cons?  l1) (if (member? (first l1) l2)
                         (merge (rest l1) l2)
                         (cons (first l1) (merge (rest l1) l2)))]))


; Design a function that checks whether a Wiki contains a path
; between two given pages.

; connected? : String String Wiki -> Boolean
; Determines whether there is a path from the first to the second page
(check-expect (connected? "Thomas"  "CS 2500"    WIKI-0) #true)
(check-expect (connected? "Faculty" "CS Classes" WIKI-0) #true)

; lookup : String Wiki -> [List-of String]
; Returns the links from the page (empty if not found)
(check-expect (lookup (page-title PAGE-0) WIKI-0) (page-links PAGE-0))
(check-expect (lookup (page-title PAGE-3) WIKI-0) (page-links PAGE-3))
(check-expect (lookup "NOT PRESENT"       WIKI-0) '())
(define (lookup page w)
  (cond
    [(empty? w) '()]
    [(cons?  w) (if (string=? (page-title (first w)) page)
                    (page-links (first w))
                    (lookup page (rest w)))]))

(define (connected? page1 page2 wiki)
  (local [(define links-from-page1 (lookup page1 wiki))
          ; link-connected : String -> Boolean
          ; #true if there is a path from s to page2 in wiki
          (define (link-connected s2)
            (connected? s2 page2 wiki))]
    (or (member? page2 links-from-page1)
        (ormap link-connected links-from-page1))))
