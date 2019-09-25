;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lecture20) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; ANNOUNCEMENTS
;; 1. You have a new partner! For the homework that is due tonight please submit with your new
;;    partner. You MUST ensure that you and your partner can submit by 5pm TONIGHT or we cannot
;;    guarantee that you will be able to submit before the deadline.
;; 2. Please remember to start the homeworks early.

;; Pop quiz! What is RESULT?

(define x 1)
(define y 3)
(define z (local [(define x 10)
                  (define y 20)]
            (local [(define x 30)]
              (+ x y))))
(define RESULT (+ x z))

;; The result is 51! The x in z is 30 but the x in RESULT is 1.

;; Let's talk about build-list. This function is on your list abstraction card but for ease of
;; reference I'll provide the signature and purpose below.

;; (X) Nat [Nat -> X] -> [List-of X]
;; Construct a list by applying f to all numbers in the range [0,n)
;(build-list n f) = (cons (f 0) (cons (f 1) (cons (f 2) ... (cons (f (sub1 n)) '()))))

;; Let's build a list of the first 10 square numbers
(define FIRST-SQUARES (build-list 10 sqr))

;; Let's build a list of 1-100
(define HUNDRED-LIST (build-list 100 add1))

;; That is WAAAAY easier than building this list manually. So you can see how build-list is
;; pretty handy when you are building up long lists.

;; Let's design a function which computes the first n double-squares (numbers that are twice as
;; large as perfect squares)

;; double-squares : Nat -> [List-of Nat]
;; Produces a list of the first n double-squares
(check-expect (double-squares 0) '())
(check-expect (double-squares 3) (list 0 2 8))
(define (double-squares n)
  (build-list n double-square))

;; double-square : Nat -> Nat
;; Produces twice the square of the given number
(check-expect (double-square 0) 0)
(check-expect (double-square 3) 18)
(define (double-square n) (* 2 (sqr n)))

;; Since our double-square function is pretty straightforward (it's only one line), and there is
;; most likely no use for it outside of double-squares, we could put it in a local. That version
;; is shown below. However, it works the same either way in this case.

;; double-squares.v2 : Nat -> [List-of Nat]
;; Produces a list of the first n double-squares
(check-expect (double-squares.v2 0) '())
(check-expect (double-squares.v2 3) (list 0 2 8))
(define (double-squares.v2 n)
  (local [;; double-square.v2 : Nat -> Nat
          ;; Produces twice the square of the given number
          (define (double-square.v2 n) (* 2 (sqr n)))]
    (build-list n double-square.v2)))

;; Let's discuss the design recipe with regards to local. Many of the steps of designing functions
;; remain the same: signature, purpose statement, and tests are all the same as they were before.
;; However, the template is a bit different. When we use a list abstraction with local we don't
;; really use the list template we generated so long ago. The template for using a list abstraction
;; with local should show us what our function will actually look like.

#;(define (abstraction-with-local-template alist)
    (local [(... some-helper-here ...)]
      (some-abstraction-here ... alist)))

;; So, let's use this to design add-3-to-all which adds 3 to every number in a list of numbers.

;; add-3-to-all : [List-of Number] -> [List-of Number]
;; Add 3 to every number in the list
(check-expect (add-3-to-all '()) '())
(check-expect (add-3-to-all (list 1 5 2)) (list 4 8 5))
(define (add-3-to-all lon)
  (local [;; add3 : Number -> Number
          ;; Add 3 to the given number
          (define (add3 n) (+ n 3))]
    (map add3 lon)))

;; See how the structure of our code now matches our template?

;; So the steps of our design recipe now look more like this:
;; 1. Signature
;; 2. Purpose statement
;; 3. Tests
;; 4. Copy in the list abstraction template
;; 5. Choose a list abstraction
;; 6. Design the local function to help the abstraction (when we say DESIGN we mean
;;     FOLLOW THE DESIGN RECIPE, so yes, you do need a signature and purpose). The
;;     only difference is that you cannot test local functions with check-expect.

;; What are some reasons why you might want to use local? One reason is that it allows you to hide
;; functions that shouldn't be available at the global level.

;; Design the function shortest-string that accepts a [List-of String] and returns the (first)
;; shortest one. If the list is empty it should return #false.

;; A MaybeString is one of:
;; - String
;; - #false
(define MS1 "hello")
(define MS2 #false)

;; maybe-string-temp : MaybeString -> ???
(define (maybe-string-temp ms)
  (cond [(string? ms) ...]
        [(false? ms) ...]))


;; shortest-string : [List-of String] -> MaybeString
;; Returns the shortest string in the list (in the event of a tie, return the first)
(check-expect (shortest-string '()) #false)
(check-expect (shortest-string (list "hello" "you" "world")) "you")
(check-expect (shortest-string (list "apple" "b" "carrot" "d")) "b")
(define (shortest-string los)
  (local [;; String MaybeString -> String
          ;; Returns the shorter string (#false should not be returned)
          (define (get-shorter s ms)
            (cond [(string? ms) (get-shorter-string s ms)]
                  [(false? ms) s]))]
    (foldr get-shorter #false los)))

;; get-shorter-string : String String -> String
;; Returns the shorter string of the two (in a tie return the first string)
(check-expect (get-shorter-string "hi" "you") "hi")
(check-expect (get-shorter-string "hello" "you") "you")
(check-expect (get-shorter-string "a" "b") "a")
(define (get-shorter-string s1 s2)
  (if (>= (string-length s2) (string-length s1)) s1 s2))

;; See how get-shorter is a function that is most likely not useful outside of the
;; context of shortest-string. It has a super specific purpose and we really don't
;; need people just calling that function all the time. All we want to give people
;; is the ability to get the shortest string.

;; BECCA: My inner function was getting a bit complicated so I called a helper which
;; I have put outside the local. I think this function is more generically useful than
;; the get-shorter function. Obviously this is a bit of a judgement call and you will
;; have to decide when you are writing code which functions are okay to put in local and
;; which are not. In lecture, Professor Mislove left this if statement inside the local.

;; What's another reason we might use local? Sometimes it allows us to do things we couldn't
;; do otherwise. For example, on Thursday we wanted to use a list abstraction but we needed
;; to use the context from our function to write it. We couldn't do that without local.

;; Design the function usd->eur that accepts a [List-of Number] representing prices in USD and
;; a number, representing the exchange rate, and converts each amount to EUR.

;; usd->eur : [List-of Number] Number -> [List-of Number]
;; Convert all the prices from USD to EUR
(check-expect (usd->eur '() 5) '())
(check-expect (usd->eur (list 2.5 3.4 9.7) 0.87) (list 2.175 2.958 8.439))
(define (usd->eur all-prices exchange-rate)
  (local [;; convert-price : Number -> Number
          ;; Convert from USD to EUR
          (define (convert-price usd) (* usd exchange-rate))]
    (map convert-price all-prices)))

;; We cannot use a list abstraction here unless we use local because we need to use the context
;; from our function (the exchange rate) inside the helper.

;; Another reason we might use local is to make it more clear what we are doing. For example...

;; Design the function slope that accepts two Posns and returns a Number representing the slope
;; of the line defined by those two points

;; slope : Posn Posn -> Number
;; Returns the slope of the line defined by these two points
(check-expect (slope (make-posn 0 0) (make-posn 4 3)) 0.75)
(check-expect (slope (make-posn 1 2) (make-posn 2 1)) -1)
(define (slope p1 p2)
  (/ (- (posn-y p2) (posn-y p1)) (- (posn-x p2) (posn-x p1))))

;; This function isn't super clear. Let's clean it up using local...

;; slope.v2 : Posn Posn -> Number
;; Returns the slope of the line defined by these two points
(check-expect (slope.v2 (make-posn 0 0) (make-posn 4 3)) 0.75)
(check-expect (slope.v2 (make-posn 1 2) (make-posn 2 1)) -1)
(define (slope.v2 p1 p2)
  (local [(define DELTA-Y (- (posn-y p2) (posn-y p1)))
          (define DELTA-X (- (posn-x p2) (posn-x p1)))]
    (/ DELTA-Y DELTA-X)))

;; There's another reason we might use local. Let's design the function mymax that accepts a
;; non-empty list of Numbers and returns the biggest. Do not use a list abstraction.

;; A [NEList-of X] is one of:
;; - (cons X '())
;; - (cons X [NEList-of X])

;; neln-template : [NEList-of Number] -> ???
(define (neln-template neln)
  (cond [(empty? (rest neln)) (... (first neln) ...)]
        [(cons? (rest neln)) (... (first neln) ...
                                  (neln-template (rest neln)) ...)]))

;; mymax : [NEList-of Number] -> Number
;; Returns the biggest element in the list
(check-expect (mymax (list 3)) 3)
(check-expect (mymax (list 3 21 8 62 4 17)) 62)
(define (mymax neln)
  (cond [(empty? (rest neln)) (first neln)]
        [(cons? (rest neln))
         (if (>= (first neln) (mymax (rest neln)))
             (first neln)
             (mymax (rest neln)))]))

;; See how in the cons case we are calculating (mymax (rest neln)) twice? We need to
;; calculate it the first time to find out which number is bigger and then again to
;; return that number. But this is inefficient! For every element we have to calculate the recursive
;; call two times! This is actually noticeably slow on even a list of 25 numbers.
;; Also we just try to avoid repeating code in general. Let's clean this up with local.

;; mymax.v2 : [NEList-of Number] -> Number
;; Returns the biggest element in the list
(check-expect (mymax.v2 (list 3)) 3)
(check-expect (mymax.v2 (list 3 21 8 62 4 17)) 62)
(define (mymax.v2 neln)
  (cond [(empty? (rest neln)) (first neln)]
        [(cons? (rest neln))
         (local [(define MAXREST (mymax (rest neln)))]
           (if (>= (first neln) MAXREST)
               (first neln) MAXREST))]))

;; Now we are only calculating this once! You can actually noticeably tell the difference.
;; Try running mymax on (build-list 25 add1) and then try running mymax.v2 on the same list.
;; See how one is so much faster than the other?

;; To recap here are some reasons we might need a local:
;; 1. Hide functions that should not be available in a global context.
;; 2. Access context from one function in another.
;; 3. Make code more readable by breaking it down into steps.
;; 4. Avoid repeated code.