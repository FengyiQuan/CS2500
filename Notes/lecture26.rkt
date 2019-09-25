;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture26) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. We will be giving up to 1.5% of extra credit for "participation". This can take several forms:
;;    - You can answer questions in class. Basically if the professor knows your name and your voice
;;      you will get some points here.
;;    - You can answer questions on Piazza. At the end of the semester we will look through Piazza
;;      to find students who answered questions WELL on Piazza. We can mark when you asked a good
;;      question or answered a question well so you should be able to see how you're doing.
;; 2. Tomorrow is election day. If you are registered in the state of MA and are going to vote
;;    (which we highly encourage), please see the Piazza post about how to make up the lab quiz.

;; Let's talk about WebPages again (we talked about this in lecture 23).

(define-struct webpage [name links])

;; A WebPage is a (make-webpage String [List-of Webpage])
;; - where name is the page's title
;; - and links is the list of links on the page

(define PAGE0 (make-webpage "A" '()))
(define PAGE1 (make-webpage "B" (list PAGE0)))
(define PAGE2 (make-webpage "C" '()))
(define PAGE3 (make-webpage "D" (list PAGE1 PAGE2)))

;; webpage-temp : WebPage -> ???
(define (webpage-temp wp)
  (... (webpage-name wp) ... (lowp-temp (webpage-links wp)) ...))

;; lowp-temp : [List-of WebPage] -> ???
(define (lowp-temp lowp)
  (cond [(empty? lowp) ...]
        [(cons? lowp)
         (... (webpage-temp (first lowp)) ...
              (lowp-temp (rest lowp)) ...)]))

;; You may recall that we talked about two webpages linking to each other and that we could not
;; represent such a thing with this data definition. How might we design a data definition where
;; we COULD represent this "loop" structure?

;; A WebPage is a (make-webpage String [List-of String])
;; - where name is the page's title
;; - and links is the list of links on the page (as represented by those pages' titles)

(define PAGE-LOOP1 (make-webpage "A" (list "B")))
(define PAGE-LOOP2 (make-webpage "B" (list "A" "X")))

;; In the pages above we also have something we call a "dangling" link: a link to a page that does
;; not yet exist.

(define PAGE-LOOP3 (make-webpage "C" (list "A")))
(define PAGE-LOOP4 (make-webpage "D" (list "A" "B" "C")))
(define PAGE-LOOP5 (make-webpage "E" '()))

;; Notice how we have a separate page (PAGE-LOOP5) that's not connected to anything

;; How would we represent a Wikipedia-like structure now? A single webpage doesn't contain all the
;; information we need.

;; A Wiki is a [List-of WebPage]
(define WIKI0 '())
(define WIKI1 (list PAGE-LOOP1 PAGE-LOOP2 PAGE-LOOP3 PAGE-LOOP4 PAGE-LOOP5))

;; Design the function num-pages that accepts a Wiki and returns the number of pages that are
;; referenced in the Wiki.

;; num-pages : Wiki -> Nat
;; Returns the number of pages that are referenced in the Wiki
(check-expect (num-pages WIKI0) 0)
#;(check-expect (num-pages WIKI1) 6)
(define (num-pages w)
  (length (get-all-pages w)))

;; get-all-pages : Wiki -> [List-of String]
;; Gets a list of all the pages in a Wiki
(check-expect (get-all-pages WIKI0) '())
#;(check-expect (get-all-pages WIKI1) (list "A" "B" "C" "D" "E" "X"))
(define (get-all-pages w)
  (cond [(empty? w) '()]
        [(cons? w)
         (append (get-all-references (first w))
                 (get-all-pages (rest w)))]))

;; get-all-references : WebPage -> [List-of String]
;; Gets a list of all the pages this page refers to (including itself)
(check-expect (get-all-references PAGE-LOOP1) (list "A" "B"))
(check-expect (get-all-references PAGE-LOOP2) (list "B" "A" "X"))
(define (get-all-references wp)
  (cons (webpage-name wp) (webpage-links wp)))

;; Obviously this doesn't sort out our uniqueness issue (which is why I've commented out some of the
;; tests). Let's fix that.

;; num-pages.v2 : Wiki -> Nat
;; Returns the number of pages that are referenced in the Wiki
(check-expect (num-pages.v2 WIKI0) 0)
(check-expect (num-pages.v2 WIKI1) 6)
(define (num-pages.v2 w)
  (length (get-all-pages.v2 w)))

;; get-all-pages.v2 : Wiki -> [List-of String]
;; Gets a list of all the pages in a Wiki
(check-expect (get-all-pages.v2 WIKI0) '())
(check-expect (get-all-pages.v2 WIKI1) (list "X" "D" "A" "B" "C" "E"))
(define (get-all-pages.v2 w)
  (cond [(empty? w) '()]
        [(cons? w)
         (merge-lists (get-all-references (first w))
                      (get-all-pages.v2 (rest w)))]))

;; merge-lists : [List-of String] [List-of String] -> [List-of String]
;; Merge two lists to find only the unique elements
;; ASSUMPTION: The second list contains only unique elements
(check-expect (merge-lists '() '()) '())
(check-expect (merge-lists '() (list "1" "2" "3")) (list "1" "2" "3"))
(check-expect
 (merge-lists (list "A" "B" "A" "B" "C") (list "A" "D" "E"))
 (list "B" "C" "A" "D" "E"))
(define (merge-lists los1 los2)
  (local [;; String [List-of String] -> [List-of String]
          ;; Add this element if it's not already in the list
          (define (add-unique elem sofar)
            (if (member? elem sofar) sofar (cons elem sofar)))]
    (foldr add-unique los2 los1)))
;; BECCA : Professor Mislove defined the function above without using list abstractions but you
;; should be able to use list abstractions when appropriate.

;; BECCA : Here's a sneaky little note. You may have noticed that I changed the order of the list
;; in my check-expect for get-all-pages.v2 (as opposed to in get-all-pages). Basically it's really
;; hard, in a problem like this, to know what order your list will come out in. I've basically
;; cheated here by copying in the actual result instead. You should NOT do that. Here's a better
;; idea: we can test that everything in the result list is an element we expect. We can use
;; this for merge-lists as well since the order there is not necessarily clear either.

;; has-same-elements? : [List-of String] [List-of String] -> Boolean
;; Do both lists have all the same elements?
(check-expect (has-same-elements? '() (list "a" "b")) #false)
(check-expect (has-same-elements? (list "a") '()) #false)
(check-expect (has-same-elements? (list "a" "b") (list "b" "a")) #true)
(check-expect (has-same-elements? (list "a" "b" "a") (list "b" "a")) #false)
(define (has-same-elements? los1 los2)
  (local [;; in-los2? : String -> Boolean
          ;; Is the given string in los2?
          (define (in-los2? s) (member s los2))]
    (and (= (length los1) (length los2))
         (andmap in-los2? los1))))

;; Now we can write some better tests:

(check-expect (has-same-elements? (get-all-pages.v2 WIKI1) (list "A" "B" "C" "D" "E" "X")) #true)
(check-expect (has-same-elements? (merge-lists (list "A" "B" "A" "B" "C") (list "A" "D" "E"))
                                  (list "A" "B" "C" "D" "E")) #true)

;; BECCA : Okay so that was a little tangent but now you know how to test things when you don't know
;; what order they will be in at the end. Which I think is useful. But as I said, this was not
;; covered in lecture so we won't expect you to make use of it on assignments or anything.
;; Let's get back on track...

;; Design the function connected? that accepts a Wiki and two Strings and returns whether or not a
;; path exists from the first to the second. You CANNOT assume that the strings given are in the
;; Wiki.

;; connected? : Wiki String String -> Boolean
;; Is there a path from p1 to p2 in the given Wiki?
#;(check-expect (connected? WIKI0 "A" "B") #false)
#;(check-expect (connected? WIKI1 "A" "X") #true)
#;(check-expect (connected? WIKI1 "C" "E") #false)

;; How can we write this function? We can check at each point whether we are directly connected to
;; the destination and if not follow the neighbors and check again. That doesn't seem like code
;; that follows the template... suspicious.

#;(define (connected? w p1 p2)
  (local [(define LINKS-FROM-START (get-page-links w p1))
          (define IS-END-IN-START-LINKS (member? p2 LINKS-FROM-START))]
    (if IS-END-IN-START-LINKS
        #true
        (any-connected? w LINKS-FROM-START p2))))

;; Okay, we definitely don't have time to finish this so take a look at those helpers on your own
;; and we will talk about them more next time.