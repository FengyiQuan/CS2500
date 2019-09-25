;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 19|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 1
;; relative->absolute : [List-of Number] -> [List-of Number]
;; takes a list of numbers which represent relative distances and convert them to absolute distances
(check-expect (relative->absolute (list)) (list))
(check-expect (relative->absolute (list 1 2 5)) (list 1 3 8))
(check-expect (relative->absolute (list 10 50 20)) (list 10 60 80))
(define (relative->absolute lon)
  (local [;; relative->absolute/a : [List-of Number] Number -> [List-of Number]
          ;; takes a list of numbers which represent relative distances
          ;; and convert them to absolute distances
          ;; Accumulator : keeps the current sum of the previous numbers in the list
          (define (relative->absolute/a lon tally)
            (cond
              [(empty? lon) '()]
              [(cons? lon) (cons (+ tally (first lon))
                                 (relative->absolute/a (rest lon) (+ tally (first lon))))]))]
    (relative->absolute/a lon 0)))

;; Exercise 2
;; Payment is one of:
;; - "cc" (Credit Card)
;; - 5 ($5 cash)
;; - 10 ($10 cash)
;; Which represents the way the customer is going to pay for tickets

;; tickets-can-sell : Nat [List-of Payment] -> Nat
;; takes in number as well as a list of payments, where the first customer is at the head of the list,
;; and returns how many tickets you will be able to sell
;; Accumulator : Adds 1 to the count of tickets that can be
;;               sold based on the list of payments if change can be made
(check-expect (tickets-can-sell 1 '()) 0)
(check-expect (tickets-can-sell 0 (list "cc" 5)) 2)
(check-expect (tickets-can-sell 1 (list "cc" 5 10)) 3)
(check-expect (tickets-can-sell 1 (list "cc" "cc" 10 10)) 3)
(check-expect (tickets-can-sell 1 (list 10 10 10 5 10 5)) 4)
(define (tickets-can-sell n lop)
  (local [(define (tickets-can-sell/a n lop num-of-tickets)
            (cond [(empty? lop) 0]
                  [(cons? lop) (num-can-sell n lop)]))]
    (tickets-can-sell/a n lop 0)))

;; num-can-sell : Nat [List-of Payment] -> Number
;; takes a number of $5 bills cashier has and a list of payment,
;; returns how many tickets you will be able to sell
(define (num-can-sell change lop)
  (cond
    [(string? (first lop)) (add1 (tickets-can-sell change (rest lop)))]
    [(and (number? (first lop)) (= (first lop) 5))
     (add1 (tickets-can-sell (add1 change) (rest lop)))]
    [(and (number? (first lop)) (= (first lop) 10))
     (if (> change 0)
         (add1  (tickets-can-sell (sub1 change) (rest lop)))
         (tickets-can-sell change (rest lop)))]))

;; Exercise 3
; A Network is a [List-of Person]
 
; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
; and represents their name, their belief, and the name of their friends
 
; A Belief is one of:
; - "blue"
; - "red"
 
(define NETWORK
  (list
   (make-person "Alice" "red" (list "Carol" "Heidi"))
   (make-person "Bob" "blue" (list "Carol" "Dan"))
   (make-person "Carol" "red" (list))
   (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace"))
   (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
   (make-person "Frank" "blue" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
   (make-person "Grace" "red" (list "Bob" "Frank"))
   (make-person "Heidi" "blue" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))

;; can-reach? : String String Network -> Boolean
;; asks the names of two people and a network asks if the first person can reach the second
(check-expect (can-reach? "Alice" "Alice" NETWORK) #t)
(check-expect (can-reach? "Alice" "Carol" NETWORK) #t)
(check-expect (can-reach? "Alice" "Bob" NETWORK) #f)
(check-expect (can-reach? "Bob" "Eric" NETWORK) #t)
(check-expect (can-reach? "Bob" "Frank" NETWORK) #t)
(check-expect (can-reach? "Bob" "Heidi" NETWORK) #f)
(define (can-reach? name1 name2 n)
  (local [;; can-reach?/a : String String Network [List-of String] -> Boolean
          ;; Accumulator: a list of the pages previously visited
          (define (can-reach?/a name1 name2 n visited)
            (cond
              [(string=? name1 name2) #t]
              [(member? name1 visited) #f]
              [(local [;; same-name2? : String
                       ;; determine if name of second person is a friend of first person
                       (define (same-name2? name)
                         (string=? name name2))]
                 (ormap same-name2? (get-friends name1 n))) #t]
              [else (local [;; gets-there? : String -> Boolean
                            ;; determine if list of person can reach to given name of
                            ;; person by given network, list of name of people and name
                            ;; of target people
                            (define (gets-there? name)
                              (can-reach?/a name name2 n (cons name1 visited)))]
                      (ormap gets-there? (get-friends-with-same-belief name1 n)))]))]
    (can-reach?/a name1 name2 n '())))

;; get-friends-with-same-belief : String Network -> [List-of String]
;; gets the friends with the same belief as the given person
(check-expect (get-friends-with-same-belief "Alice" NETWORK) (list "Carol"))
(check-expect (get-friends-with-same-belief "Alice" '()) '())
(define (get-friends-with-same-belief name n)
  (local [;; same-belief? : String -> Boolean
          (define (same-belief? friend-name)
            (string=? (people-belief n name) (people-belief n friend-name)))]
    (filter same-belief? (get-friends name n))))

;; get-friends : String Network -> [List-of String]
;; gets all friends with the given person
(check-expect (get-friends "Alice" NETWORK) (list "Carol" "Heidi"))
(define (get-friends name n)
  (cond
    [(empty? n) '()]
    [(cons? n) (if (same-name? (first n) name)
                   (person-friends (first n))
                   (get-friends name (rest n)))]))

;; people-belief : Network String -> String
;; returns someone's belief by given network and the name of this person
(check-expect (people-belief NETWORK "Alice") "red")
(check-expect (people-belief NETWORK "Heidi") "blue")
(check-expect (people-belief '() "Carol") "")
(define (people-belief n s)
  (cond
    [(empty? n) ""]
    [(cons? n) (if (string=? (person-name (first n)) s)
                   (person-belief (first n))
                   (people-belief (rest n) s))]))

;; same-name? : Person String -> Boolean
;; determine if a name of a perosn is same as given string
(check-expect (same-name? (make-person "Alice" "red" (list "Carol" "Heidi")) "Alice") #t)
(check-expect (same-name? (make-person "Alice" "red" (list "Carol" "Heidi")) "Carol") #f)
(define (same-name? p s)
  (string=? (person-name p) s))