;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture16) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. Tonight is the exam! Get excited! Tell your friends! Post about it on Facebook!
;;    The exam locations are all posted on Piazza so please check your location before
;;    you attend the exam. The exam is designed to be completed in 1 hour but you will
;;    be given three hours (from 6-9pm) to alleviate time pressure.
;; 2. We are working on getting homework 7 back to you. It will be published sometime today.

;; As you may recall from last time: FUNCTIONS ARE DATA!!! This is a very exciting new power
;; that requires us to use ISL instead of BSL. This let's us abstract functions which look
;; VERY similar but do slightly different things (as in our 'do-to-all' function from last time).

;; Here's that do-to-all function that takes in a function:

;; A ListofNumbers (LoN) is one of:
;; - '()
;; - (cons Number LoN)
(define LON0 '())
(define LON1 (cons 4 (cons 1 (cons 16 '()))))

;; lon-template : LoN -> ???
(define (lon-template lon)
  (cond [(empty? lon) ...]
        [(cons? lon) (... (first lon) ... (lon-template (rest lon)) ...)]))

;; do-to-all : LoN [Number -> Number] -> LoN
;; Apply the given function to each number in the list
(check-expect (do-to-all LON0 add1) LON0)
(check-expect (do-to-all LON1 sqrt) (cons 2 (cons 1 (cons 4 '()))))
(define (do-to-all lon operation)
  (cond [(empty? lon) lon]
        [(cons? lon) (cons (operation (first lon))
                           (do-to-all (rest lon) operation))]))

;; Then we talked about some Posn functions...

;; A Posn is a (make-posn Number Number)
(define POSN1 (make-posn -3 4))
(define POSN2 (make-posn 0 0))

;; posn-template : Posn -> ???
(define (posn-template p)
  (... (posn-x p) ... (posn-y p) ...))

;; A ListofPosns (LoP) is one of:
;; - '()
;; - (cons Posn LoP)
(define LOP0 '())
(define LOP1 (cons POSN1 (cons POSN2 '())))

;; lop-template : LoP -> ???
(define (lop-template lop)
  (cond [(empty? lop) ...]
        [(cons? lop) (... (posn-template (first lop))
                          (lop-template (rest lop)) ...)]))

;; manhattan-dist-all : LoP -> LoN
;; Produces the manhattan distance to the origin for each Posn in the list
(check-expect (manhattan-dist-all LOP0) LOP0)
(check-expect (manhattan-dist-all LOP1) (cons 7 (cons 0 '())))
(define (manhattan-dist-all lop)
  (cond [(empty? lop) lop]
        [(cons? lop) (cons (manhattan-to-origin (first lop))
                           (manhattan-dist-all (rest lop)))]))

;; manhattan-to-origin : Posn -> Number
;; Find the manhattan distance to the origin
(check-expect (manhattan-to-origin POSN1) 7)
(check-expect (manhattan-to-origin POSN2) 0)
(define (manhattan-to-origin p)
  (+ (abs (posn-x p)) (abs (posn-y p))))

;; pythagorean-dist-all : LoP -> LoN
;; Produces the pythagorean distance to the origin for each Posn in the list
(check-expect (pythagorean-dist-all LOP0) LOP0)
(check-expect (pythagorean-dist-all LOP1) (cons 5 (cons 0 '())))
(define (pythagorean-dist-all lop)
  (cond [(empty? lop) lop]
        [(cons? lop) (cons (pythagorean-to-origin (first lop))
                           (pythagorean-dist-all (rest lop)))]))

;; pythagorean-to-origin : Posn -> Number
;; Find the pythagorean distance to the origin
(check-expect (pythagorean-to-origin POSN1) 5)
(check-expect (pythagorean-to-origin POSN2) 0)
(define (pythagorean-to-origin p)
  (sqrt (+ (sqr (posn-x p)) (sqr (posn-y p)))))

;; Notice that manhattan-dist-all and pythagorean-dist-all look VERY VERY similar to do-to-all.
;; Can we abstract? We cannot because do-to-all's signature says that it takes a list of numbers
;; and a Number -> Number function. We don't have numbers, we have Posns. That's frustrating
;; because as usual we don't like repeated code, and this is a clear case of repeated code.

;; Notice that an equally valid signature for do-to-all would be

;; LoP [Posn -> Posn] -> LoP

;; So really we just want to somehow combine both of these signatures into one more generic
;; signature that allows us to give the function either Numbers or Posns.

;; BECCA: I'm about to give you some notation that was not given in the lecture because
;; I don't want you to accidentally go around writing invalid signatures. I'm sure this will
;; be covered in lecture later on so if it's confusing you'll have a chance to catch up later.

;; To do this we're going to need some new notation that allows for more generic lists:

;; A [List-of X] is one of:
;; - '()
;; - (cons X [List-of X])

;; Then we can simplify our two list data definitions to

;; A ListofNumbers (LoN) is a [List-of Number]

;; A ListofPosns (LoP) is a [List-of Posn]

;; Basically we want to indicate that we have a list where each element has the same type, and
;; that type could be anything. Note that this is DIFFERENT from [List-of Any] since in that case
;; you could provide the following list

(define LIST-OF-ANY (cons "hi" (cons 2 (cons #false (cons (make-posn 1 2) '())))))

;; We want to ensure that everything in the list has the same type so we can do the same thing to
;; every element. That's what the [List-of X] data definition ensures.

;; So let's take a look at the signature again. We want to take in a [List-of X]. Then what happens?
;; We need a function. What does that function do? It certainly takes in an element of the list
;; (so something of type X) and does something to it. What does it produce? Well we've seen that we
;; can go from Numbers to other Numbers so that would seem to indicate that it produces something of
;; type X. But we've also seen that we can go from Posns to Numbers so that indicates it would be
;; something else. We will call this thing Y. This indicates that the type COULD be different from
;; the type X, but it could also be the same. Let's put that all together.

;; do-to-all : [List-of X] [X -> Y] -> [List-of Y]

;; There's just one more thing we need to do. Since we can plug in anything for X and Y this
;; signature is basically a function of X and Y. That is, by plugging in a type for X and a type
;; for Y we get back a signature that tells us the inputs and outputs of our function. To
;; indicate that we need to plug in an X and Y we provide them in parentheses before the
;; signature like so:

;; do-to-all : (X Y) [List-of X] [X -> Y] -> [List-of Y]

;; This is known as a PARAMETERIZED signature because it has PARAMETERS (X and Y) which you
;; have to fill in to know what the inputs and outputs are. Let's use this to re-write
;; our manhattan-dist-all function.

;; manhattan-dist-all.v2 : [List-of Posn] -> [List-of Number]
;; Produces the manhattan distance to the origin for each Posn in the list
(check-expect (manhattan-dist-all.v2 LOP0) LOP0)
(check-expect (manhattan-dist-all.v2 LOP1) (cons 7 (cons 0 '())))
(define (manhattan-dist-all.v2 lop)
  (do-to-all lop manhattan-to-origin))

;; Note that in this case the X becomes Posn and the Y becomes Number.

;; Also note that I switched over to using the more generic list data definition. You should
;; really use this notation from now on, but if it's confusing to you then it's okay to keep
;; using the old notation as long as you provide all the necessary data definitions.

;; Let's talk about removing things from a list. That comes up a lot. For example, remember
;; when we had to get rid of all the moons that were off-screen? Or in lab when you had to
;; remove all the odd numbers in a list of number? Let's see if we can abstract...

(define MESSAGE1 "hello")
(define MESSAGE2 "Dear Alan, how are you today?")
(define MESSAGE3 "goodbye")
(define MESSAGE4 "Dear Becca, thank you for your hard work")
(define MESSAGE5 "CS2500")
(define MESSAGE6 "Dear Matt, you are doing a great job")

(define LOS0 '())
(define LOS1 (cons MESSAGE1 (cons MESSAGE2 (cons MESSAGE3 '()))))
(define LOS2 (cons MESSAGE4 (cons MESSAGE5 (cons MESSAGE6 '()))))

;; short-msgs : [List-of String] -> [List-of String]
;; Returns a list of all messages that are less than 14 characters
(check-expect (short-msgs LOS0) LOS0)
(check-expect (short-msgs LOS1) (cons MESSAGE1 (cons MESSAGE3 '())))
(check-expect (short-msgs LOS2) (cons MESSAGE5 '()))
(define (short-msgs all-msgs)
  (cond [(empty? all-msgs) all-msgs]
        [(cons? all-msgs)
         (if (short-msg? (first all-msgs))
             (cons (first all-msgs) (short-msgs (rest all-msgs)))
             (short-msgs (rest all-msgs)))]))

;; short-msg? : String -> Boolean
;; Is this message less than 14 characters?
(check-expect (short-msg? MESSAGE1) #true)
(check-expect (short-msg? MESSAGE2) #false)
(define (short-msg? msg) (< (string-length msg) 14))

;; Notice that we used a helper function here. There was a bit of a debate
;; about whether or not this was necessary. It certainly doesn't hurt to
;; write a helper function, and we are basically doing TWO tasks: check if
;; it's short enough and THEN either add it or don't add it. So I'd say it's
;; a good idea. The helper is pretty straightforward though (one line only)
;; so if you don't write one that would work too.

;; Also note that we put an if inside of a cond. That's okay sometimes.
;; Sometimes you can't help but put an if inside of a cond. Just remember to
;; follow these 2 rules:
;; 1. If the template calls for a helper you should use a helper
;; 2. If you are doing more than one task you should use a helper.

;; polite : [List-of String] -> [List-of String]
;; Returns a list of the messages that start with "Dear"
(check-expect (polite LOS0) LOS0)
(check-expect (polite LOS1) (cons MESSAGE2 '()))
(check-expect (polite LOS2) (cons MESSAGE4 (cons MESSAGE6 '())))
(define (polite all-msgs)
  (cond [(empty? all-msgs) all-msgs]
        [(cons? all-msgs)
         (if (polite-msg? (first all-msgs))
             (cons (first all-msgs) (polite (rest all-msgs)))
             (polite (rest all-msgs)))]))

;; polite-msg? : String -> Boolean
;; Does this string start with "Dear"?
(check-expect (polite-msg? "hi") #false)
(check-expect (polite-msg? MESSAGE1) #false)
(check-expect (polite-msg? MESSAGE2) #true)
(define (polite-msg? msg)
  (and (>= (string-length msg) 4)
       (string=? (substring msg 0 4) "Dear")))

;; Note that the helper here is a bit more complicated (we have to compute several things to figure
;; out whether the message is polite) so you absolutely do need a helper. This sort of calculation
;; would really clutter up our 'polite' function.

;; Okay, time to abstract!

;; keep-if : [List-of String] [String -> Boolean] -> [List-of String]

;; Certainly that signature works for our functions above, but is there any way to be more generic?
;; What about our function CARES if the input is a String? The function. So if we change the
;; signature for the inputted function we can parameterize...

;; keep-if : (X) [List-of X] [X -> Boolean] -> [List-of X]
;; Returns a list of elements that pass the test
(check-expect (keep-if '() even?) '())
(check-expect (keep-if LOS1 polite-msg?) (cons MESSAGE2 '()))
(define (keep-if lox test)
  (cond [(empty? lox) lox]
        [(cons? lox)
         (if (test (first lox))
             (cons (first lox) (keep-if (rest lox) test))
             (keep-if (rest lox) test))]))

;; As per our abstraction steps we will now re-write the original functions.

;; short-msgs.v2 : [List-of String] -> [List-of String]
;; Returns a list of all messages that are less than 14 characters
(check-expect (short-msgs.v2 LOS0) LOS0)
(check-expect (short-msgs.v2 LOS1) (cons MESSAGE1 (cons MESSAGE3 '())))
(check-expect (short-msgs.v2 LOS2) (cons MESSAGE5 '()))
(define (short-msgs.v2 all-msgs) (keep-if all-msgs short-msg?))

;; polite.v2 : [List-of String] -> [List-of String]
;; Returns a list of the messages that start with "Dear"
(check-expect (polite.v2 LOS0) LOS0)
(check-expect (polite.v2 LOS1) (cons MESSAGE2 '()))
(check-expect (polite.v2 LOS2) (cons MESSAGE4 (cons MESSAGE6 '())))
(define (polite.v2 all-msgs) (keep-if all-msgs polite-msg?))

;; There is one more thing we want to work on abstracting: combining all the elements of a list.
;; Let's see an example.

;; find-sum : [List-of Number] -> Number
;; Computes the sum of all numbers in the list
(check-expect (find-sum LON0) 0)
(check-expect (find-sum LON1) 21) ;;4+1+16
(define (find-sum lon)
  (cond [(empty? lon) 0]
        [(cons? lon) (+ (first lon) (find-sum (rest lon)))]))

;; find-product : [List-of Number] -> Number
;; Computes the product of all numbers in the list
(check-expect (find-product LON0) 1)
(check-expect (find-product LON1) 64) ;;4*1*16
(define (find-product lon)
  (cond [(empty? lon) 1]
        [(cons? lon) (* (first lon) (find-product (rest lon)))]))

;; Note that unlike in previous abstractions we have TWO differences here: the base case AND the
;; operation. Let's work on the abstracted version.

;; collapse : [List-of Number] Number [Number Number -> Number] -> Number

;; Again, this signature will work for all the functions we have written but we will try to be
;; more generic. We could use ANY function, as long as we can combine its output with the result
;; of the recursive call. That output must be the same as the output of the base case.

;; collapse : (X Y) [List-of X] Y [X Y -> Y] -> Y
;; Repeatedly call the given function on each element of the list
(check-expect (collapse LON0 8 -) 8)
(check-expect (collapse LON1 1 *) 64)
(define (collapse lox base-case operation)
  (cond [(empty? lox) base-case]
        [(cons? lox) (operation (first lox) (collapse (rest lox) base-case operation))]))

;; If this function is confusing to you don't worry, we'll keep talking about it for a while.
;; This is certainly a tough one to wrap your head around because it's so versatile and vague.