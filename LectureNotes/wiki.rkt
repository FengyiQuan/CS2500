;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname wiki) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct page [title links])

; A WebPage is a (make-page String [List-of String])
; A web page's title and links to other pages

#;
(define (webpage-temp wp)
  ... (page-title wp) ... (los-temp (page-links wp)) ...)

(define PAGE-0 (make-page "CCIS" (list "NEU")))
(define PAGE-1 (make-page "NEU" (list "NEU" "Boston")))
(define PAGE-2 (make-page "Boston" (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman Hall" (list "NEU")))
(define PAGE-4 (make-page "New Orleans" (list "Mardi Gras")))

; A Wiki is a [List-of WebPage]
; A list of pages in a wiki
#;
(define (wiki-temp w)
  (cond
    [(empty? w) ...]
    [(cons? w) ... (webpage-temp (first w)) ...(wiki-temp (rest w)) ...]))

(define WIKI-1 (list PAGE-0 PAGE-1 PAGE-2 PAGE-3 PAGE-4))
(define WIKI-2 (list PAGE-4))

; Design a function num-pages that accepts a Wiki
; and returns the total number of pages

;; num-pages : Wiki -> Number
(check-expect (num-pages WIKI-1) 6)
(check-expect (num-pages WIKI-2) 2)
(define (num-pages w)
  (local [;; get-all-pages : Wiki -> [Distinct List-of String]
          ;; get all the distinct pages
          (define (get-all-pages w)
            (cond
              [(empty? w) '()]
              [(cons? w) (merge (pages-on-page (first w))
                                (get-all-pages (rest w)))]))
          ;; merge : [List-of String] [Distinct List-of String] -> [Distinct List-of String]
          (define (merge newpages distinctpages)
            (cond
              [(empty? newpages) distinctpages]
              [(cons? newpages) (if (member? (first newpages) 
                                             (merge (rest newpages) distinctpages))
                                    (merge (rest newpages) distinctpages)
                                    (cons (first newpages) (merge (rest newpages) distinctpages)))]))
          ;; pages-on-page : WebPage -: [List-of String]
          ; get all the pages referenced on a page
          (define (pages-on-page wp)
            (cons (page-title wp) (page-links wp)))]
    (length (get-all-pages w))))

; design a function connected? that accepts two Strings
; representing two pages in a wiki, and a Wiki.
; It should return whether or not there is a path
; from the first page to the second
(check-expect (connected? "NEU" "CCIS" WIKI-1) #true)
(check-expect (connected? "Boston" "CCIS" WIKI-1) #true)
;(check-expect (connected? "New Orleans" "CCIS" WIKI-1) #f)

(define (connected? t1 t2 w)
  (local [(define T1-LINKS (title->links t1 w))
          (define T2-IN-T1-LINK (member? t2 T1-LINKS))
          ; hitch-a-ride? : String -> Boolean
          (define (hitch-a-ride? link)
            (connected? link t2 w))
          ; get-rid-of : String Wiki -> Wiki
          (define (get-rid-of title-to-remove oldwiki)
            (local [; is-not-bad-page? : WebPage -> Boolean
                    (define (is-not-bad-page? wp)
                      (not (webpage-matches? wp title-to-remove)))]
              (filter is-not-bad-page? oldwiki)))]
    (or T2-IN-T1-LINK
        (ormap hitch-a-ride? T1-LINKS))))
    
; title->links : string Wiki-> [List-of String]
(check-expect (title->links "asdkfjhalksdf" WIKI-1) '())
(check-expect (title->links  "Mardi Gras" WIKI-1) '())
(check-expect (title->links  "NEU" WIKI-1) (list "CCIS" "Boston"))

(define (title->links t w)
  (cond
    [(empty? w) '()]
    [(cons? w) (if
                (string=? t (page-title (first w)))
                (page-links (first w))
                (title->links t (rest w)))]))



;; broken-links? : Wiki -> Boolean
(define (broken-links? w)
  (local [; broken-link? : String -> Boolean
          (define (broken-link? link)
            (not (member? link (map page-title w))))
          ; broken-page? : WebPage -> Boolean
          (define (broken-page? wp)
            (ormap broken-link? (page-links wp)))]
    (ormap broken-page? w)))