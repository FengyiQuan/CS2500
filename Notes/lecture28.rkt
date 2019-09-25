;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture28) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A student asked about the template for a non empty list

;; A [NEList-of X] is one of:
;; - (cons X '())
;; - (cons X [NEList-of X])

;; nelist-temp : [NEList-of X] -> ???
(define (nelist-temp nelox)
  (cond [(empty? (rest nelox)) (... (x-temp (first nelox)) ...)]
        [(cons? (rest nelox))
         (... (x-temp (first nelox)) ...
              (nelist-temp (rest nelox)) ...)]))

;; REMINDER: Your second exam is coming up (it's after Thanksgiving). Get hyped.
;; If you have a conflict and we don't know about it, you should tell us. Specifically,
;; you should tell me (Becca MacKenzie). Via email. As soon as possible.

;; Okay, let's talk about Wikis again.

(define-struct webpage [name links])
;; A WebPage is a (make-webpage String [List-of String])
;; - where name is the page's title
;; - and links is the list of links on the page (as represented by those pages' titles)

(define PAGE1 (make-webpage "A" (list "B")))
(define PAGE2 (make-webpage "B" '()))
(define PAGE3 (make-webpage "C" (list "A")))
(define PAGE4 (make-webpage "D" (list "E")))
(define PAGE5 (make-webpage "E" (list "D" "F")))
(define PAGE6 (make-webpage "F" (list "G")))
(define PAGE7 (make-webpage "G" '()))

;; A Wiki is a [List-of WebPage]

(define WIKI0 '())
(define WIKI1 (list PAGE1 PAGE2 PAGE3))
(define WIKI2 (list PAGE4 PAGE5 PAGE6 PAGE7))

;; Design the function most-popular that accepts a Wiki and outputs the largest page
;; indegree (the number of incoming links) or 0 if the Wiki is empty

;; most-popular : Wiki -> Nat
;; Outputs the largest indegree in the Wiki (or 0 if the Wiki is empty)
(check-expect (most-popular WIKI0) 0)
(check-expect (most-popular WIKI1) 1)
(define (most-popular w)
  (cond [(empty? w) 0]
        [(cons? w) (max (num-inlinks w (first w))
                        (most-popular (rest w)))]))

;; There is a problem with the function above: every time we make the wiki smaller and then we try
;; to use that smaller wiki to get the number of inlinks. Let me give an example that demonstrates
;; this problem.

(define PAGE8 (make-webpage "A" (list "C")))
(define PAGE9 (make-webpage "B" (list "C")))
(define PAGE10 (make-webpage "C" '()))

(define WIKI3 (list PAGE8 PAGE9 PAGE10))

;; We want to return 2 above but our function will return 0 since by the time we get to "C" all the
;; pages that link to "C" have been removed. How can we fix this problem? We need to somehow also
;; keep track of the wiki as a whole without removing anything from it.

;; most-popular/safe : Wiki -> Nat
;; Outputs the largest indegree in the Wiki (or 0 if the Wiki is empty)
(check-expect (most-popular/safe WIKI0) 0)
(check-expect (most-popular/safe WIKI1) 1)
(check-expect (most-popular/safe WIKI3) 2)
(define (most-popular/safe w)
  (local [;; most-popular/within : Wiki -> Nat
          ;; Outputs the largest indegree given a PORTION of the wiki
          (define (most-popular/within portion)
            (cond [(empty? portion) 0]
                  [(cons? portion)
                   (max (num-inlinks w (first portion))
                        (most-popular/within (rest portion)))]))]
    (most-popular/within w)))

;; num-inlinks : Wiki Webpage -> Nat
;; Outputs the number of links to this page in the given Wiki
(check-expect (num-inlinks WIKI1 PAGE1) 1)
(check-expect (num-inlinks WIKI1 PAGE3) 0)
(define (num-inlinks w page)
  (cond [(empty? w) 0]
        [(cons? w)
         (if (links-to-page? (first w) (webpage-name page))
             (add1 (num-inlinks (rest w) page))
             (num-inlinks (rest w) page))]))

;; links-to-page? : WebPage String -> Boolean
;; Does this page link to the page with the given name?
(check-expect (links-to-page? PAGE1 "B") #true)
(check-expect (links-to-page? PAGE2 "C") #false)
(define (links-to-page? page target)
  (member? target (webpage-links page)))

;; Now let's redo that with list abstractions!

;; most-popular.v2 : Wiki -> Nat
;; Outputs the largest indegree in the Wiki (or 0 if the Wiki is empty)
(check-expect (most-popular.v2 WIKI0) 0)
(check-expect (most-popular.v2 WIKI1) 1)
(check-expect (most-popular.v2 WIKI3) 2)
(define (most-popular.v2 w)
  (local [;; Webpage Nat -> Nat
          ;; Find the max of the inlinks in the page and the given number
          (define (max-inlinks page sofar)
            (max (num-inlinks.v2 w page) sofar))]
    (foldr max-inlinks 0 w)))

;; BECCA: Professor Mislove chose to map and then foldr but I prefer to traverse the list only
;; once. Either way is fine. It's a design decision and it's up to you.

;; num-inlinks.v2 : Wiki Webpage -> Nat
;; Outputs the number of links to this page in the given Wiki
(check-expect (num-inlinks.v2 WIKI1 PAGE1) 1)
(check-expect (num-inlinks.v2 WIKI1 PAGE3) 0)
(define (num-inlinks.v2 w page)
  (local [;; Webpage Nat -> Nat
          ;; Add 1 if the given page links to page
          (define (add-if-link p sofar)
            (if (links-to-page? p (webpage-name page))
                (add1 sofar)
                sofar))]
    (foldr add-if-link 0 w)))

;; Notice how the list abstraction version is much more concise than the other version?

;; Design the function invert which accepts a Wiki and "inverts" it. In other words, if page A
;; linked to page B and B existed in the original Wiki, B should link to A in the inverted wiki.

(define PAGE1-I (make-webpage "A" (list "C")))
(define PAGE2-I (make-webpage "B" (list "A")))
(define PAGE3-I (make-webpage "C" '()))
(define WIKI1-INVERT (list PAGE1-I PAGE2-I PAGE3-I))

;; invert : Wiki -> Wiki
;; Flip all the links in the Wiki
(check-expect (invert WIKI0) WIKI0)
(check-expect (invert WIKI1) WIKI1-INVERT)
(define (invert w)
  (local [;; Webpage -> Webpage
          ;; Invert the links for the given page
          (define (invert-webpage wp)
            (make-webpage (webpage-name wp)
                          (get-inlinks w (webpage-name wp))))]
    (map invert-webpage w)))

;; get-inlinks : Wiki String -> [List-of String]
;; Returns a list of the pages that link to the given page
(check-expect (get-inlinks WIKI1 "A") (list "C"))
(check-expect (get-inlinks WIKI3 "C") (list "A" "B"))
(define (get-inlinks w pagename)
  (local [;; Webpage [List-of String] -> [List-of String]
          ;; If the given page links to pagename, add it to the list
          (define (add-if-inlink page sofar)
            (if (links-to-page? page pagename)
                (cons (webpage-name page) sofar)
                sofar))]
    (foldr add-if-inlink '() w)))