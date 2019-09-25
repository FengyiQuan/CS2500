;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |wiki 2|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(define-struct page [title links])

; A WebPage is a (make-page String [List-of String])
; A web page's title and links to other pages

#;
(define (webpage-temp wp)
  ... (page-title wp) ... (los-temp (page-links wp)) ...)

(define PAGE-0 (make-page "CCIS" (list "NEU")))
(define PAGE-1 (make-page "NEU" (list "NEU" "Boston")))
(define PAGE-2 (make-page "Boston" (list "NEU" "Shillman Hall")))
(define PAGE-3 (make-page "Shillman" (list "NEU")))
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

;; most-popular : Wiki -> Number
(check-expect (most-popular-no WIKI-1) 3)
(check-expect (most-popular-no '()) 0)

(define (most-popular-no w)
  (local [;; title-in-links? : WebPage WebPage -> Bollean
          (define (title-in-links? wp-target wp-source)
            (member? (page-title wp-target) (page-links wp-source)))
          ; indegree : WebPage Wiki -> Number
          (define (indegree wb wiki)
            (cond
              [(empty? w) 0]
              [(cons? w)
               (if (title-in-links? wb (first wiki))
                   (add1 (indegree wb (rest wiki)))
                   (indegree wb (rest wiki)))]))
          ; Wiki -> Number
          (define (helper inner-wiki)
            (cond
              [(empty? inner-wiki) 0]
              [(cons? inner-wiki) (max
                                   (indegree (first inner-wiki) w)
                                   (helper (rest inner-wiki)))]))]
    (helper w)))


(check-expect (most-popular-yes WIKI-1) 3)
(check-expect (most-popular-yes '()) 0)
(define (most-popular-yes w)
  (local [; indegree : WebPage -> Number
          (define (indegree wp)
            (local [;; title-in-links? : WebPage -> Booleam
                    (define (title-in-links? wp-source)
                      (member? (page-title wp) (page-links wp-source)))]
              (length (filter title-in-links? w))))]
    (foldr max 0 (map indegree w))))


(define OUT-0 (make-page "CCIS" (list "NEU")))
(define OUT-1 (make-page "NEU" (list "CCIS" "Boston" "Shillman")))
(define OUT-2 (make-page "Boston" (list "NEU")))
(define OUT-3 (make-page "Shillman Hall" (list "Boston")))
(define OUT-4 (make-page "New Orleans" '()))

(define (invert-wiki w)
  (local [;; invert-webpage : WebPage -> WebPage
          (define (invert-webpage wp)
            (local [;; title-in-links? : WebPage -> Boolean
                    (define (title-in-links? wp-source)
                      (member? (page-title wp) (page-links wp-source)))]
            (make-page (page-title wp)
                       (map page-title (filter title-in-links? w)))))]
    (map invert-webpage w)))