;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname path) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

; lookup : String Wiki -> [List-of String]
; Returns the links from the page (empty if not found)
(check-expect (lookup (page-title PAGE-0) WIKI-0) (page-links PAGE-0))
(check-expect (lookup (page-title PAGE-3) WIKI-0) (page-links PAGE-3))
(check-expect (lookup "NOT PRESENT"       WIKI-0) '())
(define (lookup page wiki)
  (cond
    [(empty? wiki) '()]
    [(cons?  wiki) (if (string=? (page-title (first wiki)) page)
                    (page-links (first wiki))
                    (lookup page (rest wiki)))]))

; Implement the function path that takes in two page titles and a Wiki,
; and returns a path between the two pages if one exists, or #false otherwise.

; A ListofStringorFalse is one of
; - [List-of String]
; - #false
; Interpretation: a list of strings, or #false.

; path : String String Wiki -> ListofStringorFalse
; Return a path between two pages, or #false
(check-expect (path "Faculty"    "CS Classes" WIKI-0) (list "Faculty" "Thomas" "CS Classes"))
(check-expect (path "CS Classes" "Thomas"     WIKI-0) (list "CS Classes" "CS 2500" "Thomas"))
(check-expect (path "CS 2500"    "Faculty"    WIKI-0) #false)

(define (path title1 title2 wiki)
  (local [; path-acc : String [List-of String] -> ListofStringorFalse
          ; Returns a path from from to title2 or #false if no path exits
          ; Accumulator 'visited' represents the pages visited so far
          (define (path-acc from visited)
            (cond [(string=? from title2) (list from)]
                  [(member? from visited) #false]
                  [else
                   (local [; path-helper : [List-of String] -> ListofStringorFalse
                           ; Tries to find a path from each of the given titles
                           (define (path-helper lotitle)
                             (cond [(empty? lotitle) #false]
                                   [(cons?  lotitle) (local [(define path-from-first (path-acc (first lotitle) (cons from visited)))]
                                                       (if (boolean? path-from-first)
                                                           (path-helper (rest lotitle))
                                                           path-from-first))]))
                           (define path-from-neighbor (path-helper (lookup from wiki)))]
                     (if (boolean? path-from-neighbor)
                         #false
                         (cons from path-from-neighbor)))]))]
    (path-acc title1 '())))

; Termination: this is only relevant for the path-acc function since the other functions are either
; - not recursive (like path), or
; - designed according to structural recursion (like path-helper) and thus do not need a separate termination statement (they literally always terminate *by design*).
; As for path-acc: there is only one recursive call, namely
; (path-acc (first lotitle) (cons from visited))
; Nothing is decreasing here! However:
; - When we add 'from' to 'visited' via (cons from visited), we know that 'from' is not a duplicate: we reach this call only when (member? from visited) returns false.
; - As a result, 'visited' cannot increase beyond the number of wiki pages: there are not more than this many pages!
; Together these two points show that there are never more than |wiki| many recursive calls. The function thus always terminates.
