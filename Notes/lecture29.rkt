;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture29) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/batch-io)

;; Today we're going to practice breaking down big problems into small problems. This seems like a
;; common problem that students are having, particularly with the project.

;; BECCA : Please note that we did not finish designing this program so the tests will not yet pass.
;; You should be able to run the code and see that 5 tests failed. If you finish designing the
;; prefix-each-with function, the tests should pass. We just ran out of time.


;; Design the function scrabble-words that accepts an Arrangement (representing a set of Scrabble
;; tiles) and returns a list of all valid words that can be made from those tiles.

;; An Arrangement is a [List-of 1String]
;; Representing a set of Scrabble tiles

(define ARR0 '())
(define ARR1 (list "a" "t" "c"))
(define ARR2 (list "b" "e" "x" "e"))

;; arr-temp : Arrangement -> ???
(define (arr-temp a)
  (cond [(empty? a) ...]
        [(cons? a)
         (... (first a)
              (arr-temp (rest a)) ...)]))

;; How can we do this? We need to do several things:
;; - Rearrange all the letters in all possible combinations (not necessarily using all the letters,
;;   but every possible subset of the letters in every order)
;; - Make the arrangements into words (Strings)
;; - Check if each of the words are valid English words

;; scrabble-words : Arrangement -> [List-of String]
;; Produces a list of all the valid English words that can be made with these tiles
(check-expect (scrabble-words ARR0) '())
(check-expect (same-set? (scrabble-words ARR1) (list "at" "act" "cat" "ta")) #true)
(check-expect (same-set? (scrabble-words ARR2) (list "be" "bee" "ex")) #true)

;; Note that we have to write a helper function just to write these tests. Let's write that now.

;; same-set? : (X) [List-of X] [List-of X] -> Boolean
;; Do these lists contain all the same elements?
;; NOTE: It does not matter how many times the elements appear in the list
(check-expect (same-set? '() '()) #true)
(check-expect (same-set? '() (list "a" "b" "c")) #false)
(check-expect (same-set? (list "x") '()) #false)
(check-expect (same-set? (list 2 3 1) (list 1 2 3)) #true)
(define (same-set? lox1 lox2)
  (local [;; in-lox1? : X -> Boolean
          ;; Is the given element in lox1?
          (define (in-lox1? x) (member? x lox1))

          ;; in-lox2? : X -> Boolean
          ;; Is the given element in lox2?
          (define (in-lox2? x) (member? x lox2))]
    (and (andmap in-lox1? lox2) (andmap in-lox2? lox1))))

;; How can we run these tests? We haven't implemented scrabble-words yet so those tests will
;; break! What you should NOT do is wait until you have implemented literally everything before
;; you run any of your tests. What you SHOULD do is comment out the scrabble-words tests, run
;; your function to see if the tests pass, and then go back and uncomment those tests. Just be sure
;; not to leave tests commented out as those will not be graded on an assignment.

;; BECCA: Okay, I'm just going to copy the signature, purpose, and tests down here so we can see
;; them next to our function.

;; scrabble-words : Arrangement -> [List-of String]
;; Produces a list of all the valid English words that can be made with these tiles
(check-expect (scrabble-words ARR0) '())
(check-expect (same-set? (scrabble-words ARR1) (list "at" "act" "cat" "ta")) #true)
(check-expect (same-set? (scrabble-words ARR2) (list "be" "bee" "ex")) #true)
(define (scrabble-words arr)
  (filter valid-english-word? (map arr->string (get-all-arrangements arr))))

;; So now we have made THREE wishes:
;; 1. valid-english-word? -> a function that can tell whether a word is a valid English word
;; 2. arr->string -> a function that can convert an Arrangement to a String
;; 3. get-all-arrangements -> a function that can take an Arrangement and produce all possible
;;     ordered subsets of that Arrangement

;; BECCA: The BEST thing to do when you make a wish is to IMMEDIATELY write down the signature and
;; purpose so you know what it is you're making, down the road. I'm going to do that here.

;; valid-english-word? : String -> Boolean
;; Is this word a valid English word?

;; arr->string : Arrangement -> String
;; Convert an arrangement of tiles into a String

;; get-all-arrangements : Arrangement -> [List-of Arrangement]
;; Get all possible ordered subsets of the given Arrangement

;; BECCA: Note that in top-down programming the first function is the first one we encounter in our
;; overall function. Since we read from left to right that means valid-english-word? would be first.
;; You should order things this way in your file but you can define them in any order. I am going
;; to show the functions in the order that we defined them but PLEASE make sure your functions are
;; ordered top-down in assignments.

;; Since the easiest function seems to be arr->string, let's do that one first.

;; arr->string : Arrangement -> String
;; Convert an arrangement of tiles into a String
(check-expect (arr->string ARR0) "")
(check-expect (arr->string ARR1) "atc")
(define (arr->string a)
  (foldr string-append "" a))


;; That get-all-arrangements function looks tough. Let's do that one last. So that leaves us with
;; designing the valid-english-word? function.

;; valid-english-word? : String -> Boolean
;; Is this word a valid English word?
(check-expect (valid-english-word? "cat") #true)
(check-expect (valid-english-word? "abcdefghijklmnop") #false)

;; How can we define this function. We need a list of all the valid English words. Luckily, that
;; file has been posted to Piazza already because Professor Derbinsky thought of this ahead of time!
;; This file contains all the valid Scrabble words in the English language. How can we use that to
;; see if a word is valid? We want to read the list from the file so we can use it in our function.

;(define DICTIONARY (read-dictionary "wordslower.txt"))

;; Now we have a new wish for a function read-dictionary that reads the list of words from the file.
;; BECCA: As before, write the signature and purpose so we don't forget what this function is later.

;; read-dictionary : String -> [List-of String]
;; Produces a dictionary of valid English words from the given file

;; How can we use this dictionary?
;; BECCA: Again, copying my signature, purpose, and tests, so they are with the function.

;; valid-english-word? : String -> Boolean
;; Is this word a valid English word?
(check-expect (valid-english-word? "cat") #true)
(check-expect (valid-english-word? "abcdefghijklmnop") #false)
(define (valid-english-word? w) (member? w DICTIONARY))

;; Okay, so now we need to design the read-dictionary function. Let's do that.

;; read-dictionary : String -> [List-of String]
;; Produces a dictionary of valid English words from the given file
(check-expect (read-dictionary "NOTAREALFILEPATH") '())
(define (read-dictionary fpath)
 (if (file-exists? fpath) (read-lines fpath) '()))

(define DICTIONARY (read-dictionary "wordslower.txt"))

;; BECCA: My function is a bit more complicated than the one Professor Mislove wrote. He just
;; called read-lines but I'm checking for a valid file first. Also, note that I had to move the
;; DICTIONARY definition to after the function definition since otherwise I get an error that
;; 'read-dictionary is used here before its definition'.

;; Okay, again, we want to run this to se if it works but we are calling a "get-all-arrangements"
;; function that does not yet exist. So, comment out "scrabble-words" and its tests and THEN run.
;; Then uncomment and continue.

;; One function to go!

;; get-all-arrangements : Arrangement -> [List-of Arrangement]
;; Get all possible ordered subsets of the given Arrangement
(check-expect (get-all-arrangements ARR0) (list '()))
(check-expect
 (same-set? (get-all-arrangements ARR1)
            (map explode (list "" "a" "t" "c" "at" "ac" "ta" "tc" "ca" "ct"
                               "atc" "act" "tac" "tca" "cat" "cta")))
 #true)

;; Where do we start? How do we start if we don't quite know what to do? That's exactly what the
;; TEMPLATE is for!

(define (get-all-arrangements arr)
  (cond [(empty? arr) (list '())]
        [(cons? arr) (insert-everywhere (first arr) (get-all-arrangements (rest arr)))]))

;; BECCA: As a fun exercise, see if you can write this function with a list abstraction!

;; Now we have made a wish for a new function! Let's design that function next.
;; BECCA: Even if you don't design this right away you should ALWAYS write the signature and purpose
;; right away. That is the best way to make sure all your functions work with each other later on.

;; insert-everywhere : 1String [List-of Arrangement] -> [List-of Arrangement]
;; Insert the given letter into every arrangement in the list in every possible position
;; (including not inserting it at all)
(check-expect (insert-everywhere "a" '()) '())
(check-expect
 (same-set? (insert-everywhere "x" (list '())) (list '() (list "x")))
 #true)
(check-expect
 (same-set? (insert-everywhere "b" (list '() (list "a")))
            (list '() (list "a") (list "b") (list "a" "b") (list "b" "a")))
 #true)

;; Where do we start? How do we start if we don't quite know what to do? That's exactly what the
;; TEMPLATE is for!

(define (insert-everywhere s loa)
  (cond [(empty? loa) loa]
        [(cons? loa)
         (cons (first loa)
               (append (insert-everywhere/arr s (first loa))
                       (insert-everywhere s (rest loa))))]))

;; BECCA: As a fun exercise, see if you can write this function with a list abstraction!

;; Now we have a new wish! Let's design that function next.
;; BECCA: Even if you don't design this right away you should ALWAYS write the signature and purpose
;; right away. That is the best way to make sure all your functions work with each other later on.

;; insert-everywhere/arr : 1String Arrangement -> [List-of Arrangement]
;; Insert the given string in every possible position in the given Arrangement
(check-expect (insert-everywhere/arr "a" '()) (list (list "a")))
(check-expect
 (same-set? (insert-everywhere/arr "b" (list "c" "d"))
            (list (list "b" "c" "d") (list "c" "b" "d") (list "c" "d" "b")))
 #true)

;; Where do we start? How do we start if we don't quite know what to do? That's exactly what the
;; TEMPLATE is for!

(define (insert-everywhere/arr s arr)
  (cond [(empty? arr) (list (list s))]
        [(cons? arr) (cons (cons s arr)
                           (prefix-each-with (first arr) (insert-everywhere/arr s (rest arr))))]))

;; Now we have a new wish! Let's design that next!

;; BECCA: We may not have time to design that function. But we DO have time to
;; write the signature and purpose. There is ALWAYS time to write the signature and purpose.
;; You should write the signature and purpose. When in doubt, write the signature and purpose.
;; If you are making a helper function you should have a signature and purpose. ALWAYS.

;; prefix-each-with : 1String [List-of Arrangement] -> [List-of Arrangement]
;; Prefix each arrangement with the given 1String

;; BECCA: Just so you can run this I'm going to also provide a stub. A stub is just basically a
;; fake version of the function. Our tests won't pass since I haven't actually implemented the
;; function but at least you can run the code.
(define (prefix-each-with s loa) loa) ;; TO DO: Actually design this function


;; BECCA: If you noticed that I said the same thing like four times in this file, good work. That's
;; because those things are VERY IMPORTANT. Repetition = important. So I'm hoping you will come away
;; from this lecture remembering those things more than anything. Here's a summary of the important
;; things to remember:
;; 1. When you see a big problem, don't panic. Think about it step by step. Break it down into
;;    smaller problems. Wishlist those functions. Just assume they exist and work from there. You
;;    can design those functions later.
;; 2. When you wishlist a function you should write the signature and purpose right away. Like now.
;;    It will help you ensure that it fits with your other functions later on. Students have been
;;    getting a lot of bugs where their function expects one type of data but gets another. If you
;;    follow your signatures you should not run into this type of issue.
;; 3. If you don't know how to write a function, start with the template. Templates are not just
;;    busy work we invented to make you sad. They are super useful for helping us start to design
;;    our functions when we're not sure where to go. Please use them.