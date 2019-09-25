;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 17|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
;; and represents their name, their belief, and the name of their friends

;; A Belief is one of:
;; - "blue"
;; - "red"

;; A Network is a [List-of Person]
;; Examples:
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

;; Exercise 1
;; update-network : Network -> Network
;; takes a Network and updates every individual’s belief to
;; become the belief the majority of their friends have
(check-expect (update-network NETWORK)
              (list (make-person "Alice" "red" (list "Carol" "Heidi"))
                    (make-person "Bob" "blue" (list "Carol" "Dan"))
                    (make-person "Carol" "red" (list))
                    (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
                    (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
                    (make-person "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
                    (make-person "Grace" "blue" (list "Bob" "Frank"))
                    (make-person "Heidi" "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace"))))
(define (update-network n)
  (local [;; update-network-on-previous : Network -> Network
          ;; updates network based on previous state 
          (define (update-network-on-previous previous)
            (cond
              [(empty? previous) '()]
              [(cons? previous) (cons (change-belief n (first previous))
                                      (update-network-on-previous (rest previous)))]))]
    (update-network-on-previous n)))

;; change-belief : Network Person -> Person
;; change one person's belief based on given network and this person
(check-expect (change-belief NETWORK (make-person "Alice" "red" (list "Carol" "Heidi")))
              (make-person "Alice" "red" (list "Carol" "Heidi")))
(check-expect (change-belief NETWORK (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace")))
              (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace")))
(check-expect (change-belief NETWORK (make-person "Heidi"  "blue"
                                                  (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace")))
              (make-person "Heidi"  "red" (list "Alice" "Bob" "Carol" "Dan" "Eric" "Grace")))
(define (change-belief n p)
  (cond [(> (blue-fd n (person-friends p)) (red-fd n (person-friends p)))
         (make-person (person-name p) "blue" (person-friends p))]
        [(< (blue-fd n (person-friends p)) (red-fd n (person-friends p)))
         (make-person (person-name p) "red" (person-friends p))]
        [(= (blue-fd n (person-friends p)) (red-fd n (person-friends p)))
         p]))

;; blue-fd : Network [List-of String] -> Number
;; counts the number of people whose belief is blue
(check-expect (blue-fd NETWORK '()) 0)
(check-expect (blue-fd NETWORK (list "Carol" "Heidi")) 1)
(define (blue-fd n los)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (string=? "blue" (friend-belief n (first los)))
                     (add1 (blue-fd n (rest los)))
                     (blue-fd n (rest los)))]))

;; red-fd : Network [List-of String] -> Number
;; counts the number of people whose belief is red
(check-expect (red-fd NETWORK '()) 0)
(check-expect (red-fd NETWORK (list "Carol" "Heidi")) 1)
(define (red-fd n los)
  (cond
    [(empty? los) 0]
    [(cons? los) (if (string=? "red" (friend-belief n (first los)))
                     (add1 (red-fd n (rest los)))
                     (red-fd n (rest los)))]))

;; friend-belief : Network String -> Belief
;; returns a person's belief by given the name of a person
(check-expect (friend-belief '() "Alice") "")
(check-expect (friend-belief NETWORK "Alice") "red")
(check-expect (friend-belief NETWORK "Heidi") "blue")
(define (friend-belief n name)
  (cond
    [(empty? n) ""]
    [(cons? n)
     (if (string=? name (person-name (first n)))
         (person-belief (first n))
         (friend-belief (rest n) name))]))

;; Exercise 2
;; examples:
(define ACYCLIC-NETWORK
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "red" (list "Dan" "Eric"))
   (make-person "Carol" "red" (list "Frank"))
   (make-person "Dan" "blue" (list "Carol"))
   (make-person "Eric" "red" (list "Carol"))
   (make-person "Frank" "red" (list))))

;; can-reach? : String String Network -> Boolean
;; takes the names of two people and a network asks if the first person can reach the second
(check-expect (can-reach? "Alice" "Frank" ACYCLIC-NETWORK) #t)
(check-expect (can-reach? "Carol" "Dan" ACYCLIC-NETWORK) #f)
(check-expect (can-reach? "Dan" "Carol"ACYCLIC-NETWORK) #t)
(define (can-reach? p1 p2 n)
  (local [(define ALL-FRIENDS-WITH-P1 (get-friends p1 n))
          (define SAME-BELIEF-FRIENDS-WITH-P1
            (local [;; same-belief? : String -> Boolean
                    (define (same-belief? name)
                      (string=? (people-belief n name) (people-belief n p1)))]
              (filter same-belief? ALL-FRIENDS-WITH-P1)))
          (define ARE-TWO-FRIENDS? (member? p2 ALL-FRIENDS-WITH-P1))]
    (if ARE-TWO-FRIENDS?
        #t
        (any-connected? n SAME-BELIEF-FRIENDS-WITH-P1 p2))))

;; people-belief : Network String -> String
;; returns someone's belief by given network and the name of this person
(check-expect (people-belief ACYCLIC-NETWORK "Alice") "red")
(check-expect (people-belief ACYCLIC-NETWORK "Carol") "red")
(check-expect (people-belief '() "Carol") "")
(define (people-belief n s)
  (cond
    [(empty? n) ""]
    [(cons? n) (if (string=? (person-name (first n)) s)
                   (person-belief (first n))
                   (people-belief (rest n) s))]))

;; get-friends : String Network -> [List-of String]
;; returns someone's friends by given network and the name of this person
(check-expect (get-friends "Alice" ACYCLIC-NETWORK) (list "Bob" "Carol"))
(check-expect (get-friends "Carol" ACYCLIC-NETWORK) (list "Frank"))
(check-expect (get-friends "Frank" ACYCLIC-NETWORK) (list))
(check-expect (get-friends "Frank" '()) (list))
(define (get-friends s n)
  (cond
    [(empty? n) '()]
    [(cons? n) (if (same-name? (first n) s)
                   (person-friends (first n))
                   (get-friends s (rest n)))]))

;; same-name? : Person String -> Boolean
;; determine if a name of a perosn is same as given string
(check-expect (same-name? (make-person "Alice" "red" (list "Carol" "Heidi")) "Alice") #t)
(check-expect (same-name? (make-person "Alice" "red" (list "Carol" "Heidi")) "Carol") #f)
(define (same-name? p s)
  (string=? (person-name p) s))

;; any-connected? : Network [List-of String] String -> Boolean
;; determine if list of person can reach to given name of person by given network,
;; list of name of people and name of target people
(check-expect (any-connected? ACYCLIC-NETWORK (list "Alice" "Bob") "Eric") #t)
(check-expect (any-connected? ACYCLIC-NETWORK (list "Alice" "Bob") "Frank") #t)
(check-expect (any-connected? ACYCLIC-NETWORK (list "Eric" "Frank") "Alice") #f)
(define (any-connected? n los s)
  (local [(define (reach-path? one-name)
            (can-reach? one-name s n))]
    (ormap reach-path? los)))