;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample17) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; A Person is a (make-person String Belief [List-of String])
(define-struct person [name belief friends])
;; and represents their name, their belief, and the name of their friends

;; A Belief is one of:
;; - "red"
;; - "blue"

;; A Network is a [List-of Person]

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
;; Update a network to their new beliefs
(check-expect (update-network NETWORK)
              (list
               (make-person "Alice" "red" (list "Carol" "Heidi"))
               (make-person "Bob" "blue" (list "Carol" "Dan"))
               (make-person "Carol" "red" (list))
               (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace"))
               (make-person "Eric" "red" (list "Alice" "Bob" "Carol" "Dan" "Frank" "Grace"))
               (make-person  "Frank" "red" (list "Alice" "Bob" "Carol" "Dan" "Grace"))
               (make-person  "Grace"  "blue" (list "Bob" "Frank"))
               (make-person "Heidi" "red" (list  "Alice" "Bob"  "Carol" "Dan" "Eric" "Grace"))))
(define (update-network n)
  (local [;; update-opinion : Person -> Person
          ;; Update a person's opinion
          (define (update-opinion p)
            (new-opinion p n))]
    (map update-opinion n)))

;; new-opinion : Person Network -> Person
;; Produce a person's new opinion
(check-expect (new-opinion (make-person "Alice" "red" (list "Carol" "Heidi")) NETWORK)
              (make-person "Alice" "red" (list "Carol" "Heidi")))
(check-expect (new-opinion (make-person "Bob" "blue" (list "Carol" "Dan")) NETWORK)
              (make-person "Bob" "blue" (list "Carol" "Dan")))
(check-expect (new-opinion (make-person "Carol" "red" '()) NETWORK)
              (make-person "Carol" "red" '()))
(check-expect (new-opinion (make-person "Dan" "blue" (list "Carol" "Eric" "Frank" "Grace")) NETWORK)
              (make-person "Dan" "red" (list "Carol" "Eric" "Frank" "Grace")))
(define (new-opinion p n)
  (local [;; is-friend? : Person -> Boolean
          ;; Is this person a friend of p?
          (define (is-friend? p1)
            (member? (person-name p1) (person-friends p)))
          (define friends (filter is-friend? n))
          ;; is-red? : Person -> Boolean
          ;; Does this person believe "red"?
          (define (is-red? p1)
            (string=? "red" (person-belief p1)))
          (define reds (length (filter is-red? friends)))
          (define blues (- (length friends) reds))
          (define new-belief
            (cond [(> reds blues) "red"]
                  [(< reds blues) "blue"]
                  [(= reds blues) (person-belief p)]))]
    (make-person (person-name p) new-belief (person-friends p))))

(define NETWORK/ACYCLIC
  (list
   (make-person "Alice" "red" (list "Bob" "Carol"))
   (make-person "Bob" "blue" (list "Dan"))
   (make-person "Carol" "red" (list "Dan" "Eric"))
   (make-person "Dan" "blue" (list "Eric"))
   (make-person "Eric" "red" (list))))

;; can-reach? : String String Network -> Boolean
;; Can person b be reached by person a along same-belief paths?
;; Assume no cycle
(check-expect (can-reach? "Alice" "Alice" NETWORK/ACYCLIC) #t)
(check-expect (can-reach? "Bob" "Dan" NETWORK/ACYCLIC) #t)
(check-expect (can-reach? "Alice" "Dan" NETWORK/ACYCLIC) #f)
(check-expect (can-reach? "Dan" "Carol" NETWORK/ACYCLIC) #f)
(define (can-reach? a b n)
  (local [;; can-reach? : Person -> Boolean
          ;; Can this person reach b?
          (define (can-reach? p)
            (local [;; can-friend-reach? : String -> Boolean
                    ;; Can this friend reach b?
                    (define (can-friend-reach? s)
                      (can-neighbor-reach? (get-person n s)))
                    ;; can-neighbor-reach? : Person -> Boolean
                    ;; Can this neighbor reach b?
                    (define (can-neighbor-reach? neighbor)
                      (and (string=? (person-belief p)
                                     (person-belief neighbor))
                           (can-reach? neighbor)))]
              (or (string=? (person-name p) b)
                  (ormap can-friend-reach? (person-friends p)))))]
    (can-reach? (get-person n a))))

;; get-person : Network String -> Person
;; Get the person whose name is s (assume they are present)
(check-expect (get-person NETWORK "Bob") (make-person "Bob" "blue" (list "Carol" "Dan")))
(define (get-person n s)
  (cond [(string=? s (person-name (first n))) (first n)]
        [else (get-person (rest n) s)]))