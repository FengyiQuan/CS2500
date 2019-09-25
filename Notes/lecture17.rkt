;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. Homework is due tonight! You should really also start Thursday's homework because
;;    it's another iteration of the music player project and might take some time.
;; 2. The exam went REALLY well! The average was something like a 97%. HOWEVER, please note
;;    that the class is going to increase in difficulty so doing well does NOT mean you can
;;    just slack off for the remainder of the semester.

;; As you may recall, last time we were discussing the abstraction of list functions.
;; We came up with three main activities we want to do with lists:
;; - do-to-all: Do the same thing to every element in a list
;; - keep-if: Keep only the elements of a list that pass a certain test
;; - collapse: Combine all the elements of a list in some way

;; That last one is pretty tricky. It's kind of vague what this function should be doing.
;; That's actually because it's the most abstract of all of them, so it makes sense, but
;; it also means that this is usually the one that trips people up the most. So if you think
;; it's confusing, that's pretty normal actually.

;; collapse : (X Y) [List-of X] Y [X Y -> Y] -> Y
;; Repeatedly call the given function on each element of the list
(check-expect (collapse LON0 8 -) 8)
(check-expect (collapse LON1 1 *) 64)
(define (collapse lox base-case operation)
  (cond [(empty? lox) base-case]
        [(cons? lox) (operation (first lox) (collapse (rest lox) base-case operation))]))

;; Let's use this to re-write our specific functions from last time

(define LON0 '())
(define LON1 (cons 4 (cons 1 (cons 16 '()))))

;; find-sum.v2 : [List-of Number] -> Number
;; Find the sum of all the numbers in the list
(check-expect (find-sum.v2 LON0) 0)
(check-expect (find-sum.v2 LON1) 21)
(define (find-sum.v2 lon) (collapse lon 0 +))

;; find-product.v2 : [List-of Number] -> Number
;; Find the product of all the numbers in the list
(check-expect (find-product.v2 LON0) 1)
(check-expect (find-product.v2 LON1) 64)
(define (find-product.v2 lon) (collapse lon 1 *))

;; Let's take another look at the signature for collapse

;; collapse : (X Y) [List-of X] Y [X Y -> Y] -> Y

;; How did we get here? Well, we know we need a list. It doesn't seem to matter what
;; the elements of the list are, as long as we can operate on them with our function.
;; So, we will call this kind of thing X. That gives us

;; (X) [List-of X] ??? ??? -> ???

;; The second thing and the result have to match, since the second thing is the base
;; case and we return it in the empty case. Does this thing have to be the same as X?
;; Well, in our examples above it does happen to be the same but it turns out we can
;; choose anything we like so we will call this thing Y. That gives us

;; (X Y) [List-of X] Y ??? -> Y

;; Now we have to figure out the signature of our input function. Well, it takes the
;; first element of the list which is of type X, and it takes the result of the
;; recursive call which, since our function's result is of type Y, must be of type Y.
;; It has to return whatever the result of our function is (Y), so we get

;; (X Y) [List-of X] Y [X Y -> Y] -> Y

;; Okay, let's do some practice with this function.

(define LOS0 '())
(define LOS1 (cons "hello" (cons "world" '())))

;; total-length : [List-of String] -> Nat
;; Produces the total length of all the strings in the list
(check-expect (total-length LOS0) 0)
(check-expect (total-length LOS1) 10)
(define (total-length los)
  (collapse los 0 add-length-to-num))

;; add-length-to-num : String Nat -> Nat
;; Add the length of the given string to the given number
(check-expect (add-length-to-num "" 10) 10)
(check-expect (add-length-to-num "hello" 2) 7)
(define (add-length-to-num str num)
  (+ (string-length str) num))

;; Note that we needed to create a helper function here because we didn't already have
;; a function that adds a string's length to the sum of the lengths so far. We have to
;; pass in such a function so we had to create one.

;; Okay, collapse is a pretty confusing function due to its totally vague purpose and ability
;; to do basically whatever you want with a list. You can think about it like this.
#;(collapse (cons a (cons b (cons c '()))) base f)
;; Is the same as
#;          (f    a (f    b (f    c base)))
;; So, all the 'cons' calls became 'f' and the empty list became the base case
;; BECCA: Sorry for the weird indentation but I'm trying to show the pattern in a readable way.
;;  You should absolutely not use this weird indentation on your homework.

;; So what happens if we do (collapse LON cons '())? Well, we replace all the 'cons' calls with
;; the function, which is 'cons', so nothing changes there. Then we replace the empty list with
;; the base case, which is the empty list, so nothing changes there. So we just get back the
;; same list again!

;; BECCA: Here's where we talked about [List-of X] notation. So you're about one or two lectures
;; ahead here if you've been reading these notes. But I just didn't want you to use the wrong
;; notation on your homework ever. You should ALWAYS be using the [List-of X] notation when
;; dealing with generic lists.

;; Okay, it seems like we're going to be dealing with a lot of lists. I don't know about you, but
;; personally I am tired of writing cons a million times in order to create these lists. Here's
;; some shorthand to help you out:

(define MY-LIST (list 1 2 3 4 5))
;; This is equivalent to (cons 1 (cons 2 (cons 3 (cons 4 (cons 5 '())))))

(define MY-OTHER-LIST (list "a" "b" "c"))
;; This is equivalent to (cons "a" (cons "b" (cons "c" '())))

;; Note that list and cons are NOT the same and CANNOT be used interchangeably. Here's the
;; signatures and purpose statements to help you remember this:

;; cons : X [List-of X] -> [List-of X]
;; Add a SINGLE element to the front of an already existing list

;; list : X X .... -> [List-of X]
;; Create a new list with all the given elements

;; Here's some more examples of using this shorthand
(define EXAMPLE1 (list 1)) ;; (cons 1 '())
(define EXAMPLE2 (list 3 "hello" (make-posn 0 8)))
;; (cons 3 (cons "hello" (cons (make-posn 0 8) '())))
(define EXAMPLE3 (list (list 1 2) (list 3)))
;; (cons (cons 1 (cons 2 '())) (cons 3 '()))
;; This one's pretty cool. Lists inside of lists! Crazy stuff.
(define EXAMPLE4 (list "a" 0 #false))
;; (cons "a" (list 0 #false))
;; See how we can use cons and list together? Because of the signature of cons
;; (it takes an element AND a list) and the signature of list (it produces a list)
;; we can use them together to create lists.

;; Please be careful when using 'list' because as I said before the two functions
;; are NOT interchangeable and using one when you mean the other can have disastrous
;; consequences.

;; You no longer have to use 'cons' but keep in mind that this is just shorthand. Underneath
;; it all the list is still built with 'cons'.

;; BECCA: If you're not confident that you understand the difference between 'list' and 'cons',
;; I recommend using 'cons' because there is RARELY a case where you MUST use 'list' and that way
;; you don't accidentally use the wrong one.

;; Okay, let's talk about list abstractions some more. It turns out that ISL comes with some
;; built-in list abstractions:

;; map : (X Y) [X -> Y] [List-of X] -> [List-of Y]
;; Do the same thing to every element of the list (we called this 'do-to-all')

;; filter : (X) [X -> Boolean] [List-of X] -> [List-of X]
;; Remove elements that do not pass the given test (we called this 'keep-if')

;; foldr : (X Y) [X Y -> Y] Y [List-of X] -> Y
;; Fold the elements of the list together into one result (we called this 'collapse')

;; Let's use these to design add-7-to-all which takes a [List-of Number] and adds 7 to every
;; number in the list. Which abstraction is useful here?
;; We want to do the same thing to every element in the list which is exactly what map does.

;; add-7-to-all : [List-of Number] -> [List-of Number]
;; Add 7 to every element of the list
(check-expect (add-7-to-all LON0) LON0)
(check-expect (add-7-to-all LON1) (list 11 8 23))
(define (add-7-to-all lon)
  (map add-7 lon))

;; add-7 : Number -> Number
;; Add 7 to the given number
(check-expect (add-7 8) 15)
(check-expect (add-7 0) 7)
(define (add-7 n) (+ 7 n))

;; Again, we had to create a helper function because we didn't already have a function that can
;; do what we want to each element of the list.

;; Let's do another one! Let's design a function that takes a list of numbers and keeps only the
;; negative numbers.
;; Which abstraction should we use? Well, we want to take a list, and REMOVE some elements. That
;; sounds like filter!

;; only-negatives : [List-of Number] -> [List-of Number]
;; Return a list of all the negative numbers in the given list
(check-expect (only-negatives LON0) LON0)
(check-expect (only-negatives (list -3 5 -7 pi)) (list -3 -7))
(define (only-negatives lon)
  (filter negative? lon))

;; Let's do another one! Let's design a function which takes a list of booleans and returns true if
;; any of them are #true.
;; Which abstraction should we use? Well, our function is returning a Boolean, as opposed to a list.
;; We only know one abstraction which can return a non-list thing: foldr.

;; (Some people were very clever and looked at their cards and discovered that ormap would also
;; work, but we haven't talked about that one yet so we're going to use foldr instead).

;; any-true? : [List-of Boolean] -> Boolean
;; Are any of the booleans #true?
(check-expect (any-true? '()) #false)
(check-expect (any-true? (list #false #true #false)) #true)
(define (any-true? lob)
  (foldr is-either-true? #false lob))

;; You might think we could use 'or' here but it turns out 'or' isn't really a function, it's a
;; macro. The distinction here is not something we will get into but just try using 'or' and
;; you can see that it breaks.

;; is-either-true? : Boolean Boolean -> Boolean
;; Is either Boolean true?
(check-expect (is-either-true? #true #true) #true)
(check-expect (is-either-true? #true #false) #true)
(check-expect (is-either-true? #false #true) #true)
(check-expect (is-either-true? #false #false) #false)
(define (is-either-true? b1 b2) (or b1 b2))