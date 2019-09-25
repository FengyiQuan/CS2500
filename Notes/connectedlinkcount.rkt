;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname a) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct page [title links])

; A Page is a (make-page String [List-of String])
; Interpretation: web page's title and links to other pages.
(define PAGE-0 (make-page "CS 2500"    (list "Thomas")))
(define PAGE-1 (make-page "CS Classes" (list "CS 2500")))
(define PAGE-2 (make-page "Thomas"     (list "CS 2500" "CS Classes")))
(define PAGE-3 (make-page "Faculty"    (list "Thomas")))
#;
(define (page-temp page)
  ... (page-title page) ... (los-temp (page-links page)))

; A Wiki is a [List-of Page]
; Interpretation: A list of pages in a wiki
(define WIKI-0 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3))
#;
(define (wiki-temp wiki)
  (cond
    [(empty? wiki) ...]
    [(cons?  wiki) ... (page-temp (first wiki)) ... (wiki-temp (rest wiki)) ...]))


; Design a function connected? that checks whether
; a Wiki contains a path between two given pages.

; connected? : String String Wiki -> Boolean
; Determines whether there is a path from the first to the second page
(check-expect (connected? "Thomas"  "CS 2500"    WIKI-0) #true)
(check-expect (connected? "Faculty" "CS Classes" WIKI-0) #true)
(check-expect (connected? "Thomas" "Faculty"     WIKI-0) #false)
(define (connected? title1 title2 wiki)
  (connected?-helper title1 title2 wiki '()))

; connected?-helper : String String Wiki [List-of String] -> Boolean
; checks connectivity given the list of already explored pages
(define (connected?-helper title1 title2 wiki explored)
  (if (member? title1 explored)
      #false
      (local [(define links-from-title1 (lookup title1 wiki))
              ; link-connected? : String -> Boolean
              ; #true if there is a path from s to title2 in wiki
              (define (link-connected? s2)
                (connected?-helper s2 title2 wiki (cons title1 explored)))]
        (or (member? title2 links-from-title1)
            (ormap link-connected? links-from-title1)))))

; lookup : String Wiki -> [List-of String]
; Returns the links from the page (empty if not found)
(check-expect (lookup (page-title PAGE-0) WIKI-0) (page-links PAGE-0))
(check-expect (lookup (page-title PAGE-3) WIKI-0) (page-links PAGE-3))
(check-expect (lookup "NOT PRESENT"       WIKI-0) '())
(define (lookup title wiki)
  (cond
    [(empty? wiki) '()]
    [(cons?  wiki) (if (string=? (page-title (first wiki)) title)
                    (page-links (first wiki))
                    (lookup title (rest wiki)))]))


; Warmup exercise:

; Design a list abstraction function count-pred that takes a predicate
; and a list of something and counts how many elements satisfy the predicate.

; count-pred : (X) [X -> Boolean] [List-of X] -> Nat
; count elements satisfying this predicate
(check-expect (count-pred even?   (list 1 2 3 4 5))     2)
(check-expect (count-pred number? (list '() 4 "Hello")) 1)
(define (count-pred pred lox)
  (cond [(empty? lox) 0]
        [(cons?  lox) (local [(define count-pred-rest (count-pred pred (rest lox)))]
                        (if (pred (first lox))
                            (add1 count-pred-rest)
                            count-pred-rest))]))

; Design a function all-link-counts that computes, for each page in a wiki,
; how many links point to it.

; Design a suitable data definition to represent this information!

(define-struct link-count [title count])

; A Link-Count is a (make-link-count String Nat)
; where title is the name of a webpage and count is how many times it is linked to
(define LC-CS-2500 (make-link-count "CS 2500" 2))
#;
(define (lc-temp lc)
  (link-count-title lc) ... (link-count-count lc))

(define LCS-0 (list (make-link-count "CS 2500"    2)
                    (make-link-count "CS Classes" 1)
                    (make-link-count "Thomas"     2)
                    (make-link-count "Faculty"    0)))

; all-link-counts : Wiki -> [List-of Link-Count]
; compute the link counts for all wiki pages
(check-expect (all-link-counts WIKI-0) LCS-0)
(define (all-link-counts wiki)
  (local [; link-count : Page -> Link-Count
          ; compute the link-count for given page
          (define (link-count to-page)
            (local [; is-to-page-linked :Page -> Boolean
                    ; true iff to-page is linked from that page
                    (define (is-to-page-linked? from-page)
                      (member? (page-title to-page) (page-links from-page)))]
              (make-link-count (page-title to-page) (count-pred is-to-page-linked? wiki))))]
    (map link-count wiki)))
