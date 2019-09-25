;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture32) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;;BECCA: Welcome back! I hope you all had a lovely break and are feeling refreshed and ready
;; for the final part of the semester! There are only 5 more lectures until the end of the semester!

;; ANNOUNCEMENTS
;; 1. There is homework due tomorrow. There is also homework due a week from today.
;; 2. There is an exam on Wednesday, from 6-9pm.
;; 3. If you lost/misplaced/destroyed your cards you can add the material from them onto your cheat
;;    sheet. We are not giving out extra cards. The material from the cards is on the course page.
;; 4. You may bring a single sheet of paper, front and back, written or typed, to the exam. You may
;;    also bring your informational cards if you still have them.
;; 5. The exam will cover everything starting with lists and ending just before generative recursion

;; In the past few weeks we have been discussing problems we can't solve structurally either because
;; it's too slow or because it simply won't work.

;; There's another way to solve these problems. It's come up a few times. How do we know that
;; our structurally recursive code terminates? We make the data smaller every time until we hit the
;; smallest possible case. But there is another way! We can make some data bigger every time until
;; we hit some BIGGEST possible case. This kind of function is called an ACCUMULATOR FUNCTION.

;; Another way to think of accumulator functions is that our functions in the past have had no
;; memory. In other words, when going through a list of web pages we couldn't know whether we
;; had been at a web page before. We had no memory of what we had done before. We are going to
;; give you that power now.

;; Let's imagine we have a list of relative distances between cities along a road and we want to
;; convert them to a list of absolute distances from the first city.

;; relative->absolute : [List-of Number] -> [List-of Number]
;; Convert from relative to absolute distances
(check-expect (relative->absolute '()) '())
(check-expect (relative->absolute (list 10)) (list 10))
(check-expect (relative->absolute (list 10 50 20)) (list 10 60 80))
(define (relative->absolute lon)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (first lon) (add-to-all (first lon) (relative->absolute (rest lon))))]))

;; add-to-all : Number [List-of Number] -> [List-of Number]
;; Add the given number to each number in the list
(check-expect (add-to-all 27 '()) '())
(check-expect (add-to-all 6 (list 2 8 3)) (list 8 14 9))
(define (add-to-all n lon)
  (local [;; add-to-one : Number -> Number
          ;; Add n to the given number
          (define (add-to-one num) (+ n num))]
    (map add-to-one lon)))

;; Let's take a look at the performance of this.
#;(relative->absolute (build-list 20000 add1))
;; This is very noticeably slow. In fact, in class we saw a noticeable slow down at even a list
;; of 1000 elements. And when we doubled the length of the list the time multiplied by 4! That's
;; terrible performance.

;; How would we do this in real life? We would take the first number, add it to the second number,
;; then add that to the third number, etc. So at each point we remember the SUM of the past numbers.
;; To keep track of this we need an extra argument

;; relative->absolute.v2 : [List-of Number] Number -> [List-of Number]
;; Convert from relative to absolute distances
(check-expect (relative->absolute.v2 '() 17) '())
(check-expect (relative->absolute.v2 (list 10) 8) (list 18))
(check-expect (relative->absolute.v2 (list 10 50 20) 0) (list 10 60 80))
(define (relative->absolute.v2 lon sofar)
  (cond [(empty? lon) '()]
        [(cons? lon)
         (cons (+ (first lon) sofar)
               (relative->absolute.v2 (rest lon) (+ (first lon) sofar)))]))

;; But this doesn't follow the signature we were meant to have! How can we "hide" this accumulator
;; so that our outer function still only takes a [List-of Number]? We can use a local.

;; relative->absolute.v3 : [List-of Number] -> [List-of Number]
;; Convert from relative to absolute distances
(check-expect (relative->absolute.v3 '()) '())
(check-expect (relative->absolute.v3 (list 10)) (list 10))
(check-expect (relative->absolute.v3 (list 10 50 20)) (list 10 60 80))
(define (relative->absolute.v3 lon)
  (local [;;relative->absolute/a : [List-of Number] Number -> [List-of Number]
          ;; Convert from relative to absolute distances given a distance so far
          ;; ACCUMULATOR: The accumulator is the total distance so far
          (define (relative->absolute/a lon n)
            (cond [(empty? lon) '()]
                  [(cons? lon)
                   (cons (+ n (first lon)) (relative->absolute/a (rest lon) (+ n (first lon))))]))]
    (relative->absolute/a lon 0)))

;; By convention we call this inner function "some function name"/a where the 'a' stands for
;; ACCUMULATOR. We also MUST provide an ACCUMULATOR STATEMENT which describes what the accumulator
;; is accumulating.

;; When you're using an accumulator your code will look something like this...

#;(define (my-function-name some inputs here)
    (local [;; my-function-name/a : signature
            ;; purpose
            ;; ACCUMULATOR: state what the accumulator is accumulating
            (define (my-function-name/a some inputs here and-an-accumulator)
              ...)]
      (my-function-name/a some inputs here base-case-for-accumulator)))

;; Let's think back to our connected? function from lecture 30.
;; Can we rewrite this function using accumulators? What would the accumulator be?

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
(define PAGE8 (make-webpage "X" (list "Y" "Z")))
(define PAGE9 (make-webpage "Y" (list "X" "Z")))
(define PAGE10 (make-webpage "Z" (list "Q")))
(define PAGE11 (make-webpage "Q" '()))

;; A Wiki is a [List-of WebPage]

(define WIKI0 '())
(define WIKI1 (list PAGE1 PAGE2 PAGE3))
(define WIKI2 (list PAGE4 PAGE5 PAGE6 PAGE7))
(define CYCLIC-WIKI (list PAGE8 PAGE9 PAGE10 PAGE11))

;; connected? : Wiki String String -> Boolean
;; Is there a path from p1 to p2 in the given Wiki?
(check-expect (connected? WIKI0 "A" "B") #false)
(check-expect (connected? WIKI1 "C" "B") #true)
(check-expect (connected? WIKI1 "C" "E") #false)
(check-expect (connected? CYCLIC-WIKI "X" "Q") #true)
(define (connected? w p1 p2)
  (local [;; connected?/a : String String [List-of String] -> Boolean
          ;; Is there a path from p1 to p2 not including any of the given pages?
          ;; ACCUMULATOR: A list of all the pages already visited
          (define (connected?/a p1 visited)
            (cond [(string=? p1 p2) #true]
                  [(member? p1 visited) #false]
                  [else
                   (local [;; connected-neighbor? : String -> Boolean
                           ;; Is this neighbor connected to p2?
                           (define (connected-neighbor? neighbor)
                             (connected?/a neighbor (cons p1 visited)))]
                     (ormap connected-neighbor? (get-neighbors p1 w)))]))]
    (connected?/a p1 '())))

;; BECCA: For consistency, Professor Mislove's connected?/a function took in all the same arguments
;; as connected? and an accumulator as well. However, you should notice that a couple of these
;; inputs don't change throughout the course of the program. We only need our function to take in
;; the things that change over time so I have simplified above.

;; BECCA: Our local function above has its own local function. That's something I tend to discourage
;; because at that point it's clear your function is pretty complex if it's using its own local
;; function. But with accumulator functions you ALWAYS want to hide the accumulator function in a
;; local so there's not much we can do about it.

;; get-neighbors : String Wiki -> [List-of String]
;; Produce the neighbors of the given web page
;; Produces an empty list if the web page is not found
(check-expect (get-neighbors "not here" '()) '())
(check-expect (get-neighbors "E" WIKI2) (list "D" "F"))
(define (get-neighbors page-name w)
  (cond [(empty? w) '()]
        [(cons? w)
         (if (same-page-name? (first w) page-name)
             (webpage-links (first w))
             (get-neighbors page-name (rest w)))]))

;; same-page-name? : WebPage String -> Boolean
;; Does the given page have the given name?
(check-expect (same-page-name? PAGE1 "A") #true)
(check-expect (same-page-name? PAGE2 "C") #false)
(define (same-page-name? wp page-name)
  (string=? (webpage-name wp) page-name))