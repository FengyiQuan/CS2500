;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname review-session-2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

;; 2017 Problem 1

; A PB is one of:
; -- '()
; -- (cons String (cons Number PB))
; interpretation A phone book such as
; (cons "Alan" (cons 617738.1212 pb))
; means "Alan"'s phone number is 617738.1212,
; and the rest of the phone book is pb

(define PB1 '())
(define PB2 (cons "Amogh" (cons 123456789 '())))
(define PB3 (cons "Deepak" (cons 987654321 PB2)))
(define PB4 (cons "Amogh" (cons 1111111111 PB3)))


#;(define (pb-temp pb)
    (cond [(empty? pb) ... pb ...]
          [(cons? pb) ... (first pb)
                      ... (first (rest pb)) ; or (second pb)
                      ... (pb-temp (rest (rest pb)))])) ; this has no alternatives :(


;; remove-both: String PB -> PB
;; Removes all occurences of n and the associated phone number.

;(remove-both "name" PB1) -> PB1
;(remove-both "Amogh" PB2) -> '()
;(remove-both "Amogh" PB3) -> (cons "Deepak" (cons 987654321 '()))
;(remove-both "Amogh" PB4) -> (cons "Deepak" (cons 987654321 '()))

(check-expect (remove-both "name" PB1) PB1)
(check-expect (remove-both "Amogh" PB2) '())
(check-expect (remove-both "Amogh" PB3) (cons "Deepak" (cons 987654321 '())))
(check-expect (remove-both "Amogh" PB4) (cons "Deepak" (cons 987654321 '())))

(define (remove-both n pb)
  (cond [(empty? pb) '()] ; pb is also fine
        [(cons? pb) (if (string=? (first pb) n)
                        (remove-both n (rest (rest pb)))
                        ;(cons String (cons Number PB))
                        (cons (first pb) (cons (first (rest pb))
                                               (remove-both n (rest (rest pb))))))]))



;; 2017 problem 2

; A Count is a (make-count Natural Natural)
; INTERPRETATION: the number of exact multiples of a number
; and the number of non-multiples in some collection of numbers
(define-struct count [multiples leftovers])

#;(define (count-temp count)
    (... (count-multiples count) ... (count-leftovers count) ...))

;; counts-of-multiples : [List-of Nat] Nat -> Count
;; Take inspiration from problem statement.

(check-expect (counts-of-multiples (list 1 2 3 4 5 6) 2) (make-count 3 3))
(check-expect (counts-of-multiples '() 3) (make-count 0 0))

(define (counts-of-multiples lon n)
  (local [;; update-count : Nat Count -> Count
          ;; Updat count based on whether the number is a multiple or not.
          (define (update-count nat count)
            (if (zero? (remainder nat n))
                (make-count (add1 (count-multiples count)) (count-leftovers count))
                (make-count (count-multiples count) (add1 (count-leftovers count)))))]
    (foldr update-count (make-count 0 0) lon)))


;; 2016 Problem 2

;; drop: [List-of Posn] -> [List-of Posn]
;; Copy from problem statement

(check-expect (drop (list (make-posn 0 200) (make-posn 0 201)))
              (list (make-posn 0 203)))
(check-expect (drop '()) '())

(define (drop lop)
  (local [;; helper : Posn [List-of Posn] -> [List-of Posn]
          ;; Posn whose y coordinate is larger than 200
          ;; is removed. All other y coordinates are increased by 3.
          (define (helper p l)
            (if (> (posn-y p) 200)
                l ;; not including p
                (cons (make-posn (posn-x p) (+ 3 (posn-y p))) l)))] ;; include p
    (foldr helper '() lop)))


(define (drop2 lop)
  (cond [(empty? lop) '()]
        [(cons? lop) (if (> (posn-y (first lop)) 200)
                         (drop2 (rest lop))
                         (cons (make-posn (posn-x (first lop))
                                          (+ 3 (posn-y (first lop))))
                               (drop2 (rest lop))))]))



;; 2017 Problem 3

; A Shrub is one of
; - Number
; - [List-of Shrub]
; INTERPRETATION: Describes a branching garden plant, either
; the size of a leaf (in inches), or a fork with an arbitrary
; number of branches coming off it

#;(define (shrub-temp shrub)
    (cond [(number? shrub) ...]
          [(list? shrub) ... (los-temp shrub)]))

#;(define (los-temp los)
    (cond [(empty? los) ...]
          [(cons? los) ... (shrub-temp (first los))
                       ... (los-temp (rest los))]))


; A Shrub is one of
; - Number
; - [List-of Shrub]
; INTERPRETATION: Describes a branching garden plant, either
; the size of a leaf (in inches), or a fork with an arbitrary
; number of branches coming off it



;; max-branches : Shrub -> Number
;; computes the largest number of branches coming
;; out of a fork in a Shrub

(check-expect (max-branches 5) 0)
(check-expect (max-branches '()) 0)
(check-expect (max-branches (list 1 2 3 4)) 4)
(check-expect (max-branches (list (list 1 2)
                                  (list 1 2 3 4))) 4)
(check-expect (max-branches (list (list 1 2)
                                  (list 1
                                        (list 1 2 3 4 5 6)))) 6)

(define (max-branches shrub)
  (cond [(number? shrub) 0]
        [(list? shrub) (max (length shrub)
                            (max-branches-los shrub))])) ;; Wish for this function
;; to find the largest fork
;; in the list of shrubs


;; max-branches-los : [List-of Shrub] -> Number
;; Fulfills my wish :)

(check-expect (max-branches-los '()) 0)
(check-expect (max-branches-los (list 1 2 3 4)) 0)
(check-expect (max-branches-los (list (list 1 2)
                                      (list 1 2 3 4))) 4)
(check-expect (max-branches-los (list (list 1 2)
                                      (list 1
                                            (list 1 2 3 4 5 6)))) 6)

(define (max-branches-los los)
  (local [;; helper : Shrub Number -> Number
          ;; Find out the size of the largest fork
          ;; HINT: "Y" in the helper is always the answer so far
          (define (helper shrub max-so-far)
            (max (max-branches shrub) max-so-far))]
    (foldr helper 0 los))
  #;(cond [(empty? los) 0]
          [(cons? los) (max (max-branches (first los))
                            (max-branches-los (rest los)))]))



;; 2016 Problem 3

(define LEFT-WING "(")
(define RIGHT-WING ")")

; A Butterfly is one of:
; -- "body"
; -- (list LEFT-WING Butterfly RIGHT-WING)

#;(define (butterfly-temp btfly)
    (cond [(string? btfly) ...]
          [(list? btfly) ... (first btfly)
                         ... (butterfly-temp (second btfly))
                         ... (third btfly)])) ;; can use else also


;; count-wings : Butterfly -> Nat
;; counts the pairs of wings that surround butterfly's body.

(check-expect (count-wings "body") 0)
(check-expect (count-wings (list LEFT-WING "body" RIGHT-WING)) 1)
(check-expect (count-wings (list LEFT-WING
                                 (list LEFT-WING "body" RIGHT-WING)
                                 RIGHT-WING)) 2)

(define (count-wings btfly)
  (cond [(string? btfly) 0]
        [(list? btfly) (add1 (count-wings (second btfly)))]))




;; 2017 Problem 4

; A StringExpr is one of
; - String
; - (list StringExpr "+" StringExpr) ;; NOT A LIST-OF STRING

#;(define (string-expr-temp string-expr)
    (cond [(string? string-expr) ...]
          [(list? string-expr) ... (string-expr-temp (first string-expr))
                               ... (second string-expr)
                               ... (string-expr-temp (third string-expr))]))

;; combine : StringExpr -> String
;; Copy from problem statement

(check-expect (combine "hi") "hi")
(check-expect (combine (list "hi" "+" "hello")) "hihello")
(check-expect (combine (list "hi" "+" (list "hello" "+" "Dosvidaniya")))
              "hihelloDosvidaniya")

(define (combine string-expr)
  (cond [(string? string-expr) string-expr] ;; No strings to combine, so we return the one we have
        [(list? string-expr) (string-append (combine (first string-expr)) ;; replaced string-expr-temp with combine
                                            ;                                             (second string-expr)             we don't need the "+" so we can ignore it      
                                            (combine (third string-expr)))]));; same recursive call





; 2016 Problem 4


;; KEY IDEA: This function needs correct use of templates. As long as you write the templates
;;           correctly (requires great care and affection) and follow them through, you will
;;           succeed.


(define-struct bullets (loi))
(define-struct points (loi))
(define-struct item (low))
; An Enumeration is one of:
; -- (make-bullets LoI) ;; bulletized items
; -- (make-points LoI) ;; numbered points

#;(define (enum-temp enum)
    (cond [(bullets? enum) ... (loi-temp (bullets-loi enum))] ; "bullets" does not have its own template, so we break it down here
          [(points? enum) ... (loi-temp (points-loi enum))])) ; same as above


; An LoI is a [List-of Item]

;; The classic list template returns for LoI
#;(define (loi-temp loi)
    (cond [(empty? loi) ...]
          [(cons? loi) ... (item-temp (first loi))
                       ... (loi-temp (rest loi))]))
;
; An Item is a structure: (make-item LoW)

;; This is a rather simple instance 
#;(define (item-temp i)
    (... (low-temp (item-low i)) ...))


; An LoW is a [List-of Word]

;; The classic list template returns for LoW as well
#;(define (low-temp low)
    (cond [(empty? low) ...]
          [(cons? low) ... (word-temp (first low))
                       ... (low-temp (rest low))]))

; A Word is one of:
; -- String
; -- Enumeration

;; Word is n enumeration, so we'll need a cond. Callback to Traffic Light template.
#;(define (word-temp w)
    (cond [(string? w) ...]
          [else ... (enum-temp w) ...]))

;; That's 5 templates in total. Now all we have to do is follow the templates to the solution.


; interpretation An Enumeration is a generic data
; representation of HTML, LateX, etc nested,
; itemized lists.


;; cleanse : Enumeration -> Enumeration
;; Remove foul language from an enumeration.

(check-expect (cleanse (make-bullets (list (make-item (list "This" "Is" "a" "Sentence"))
                                           (make-item (list "@#$%" "is" "a" "bad" "word"))
                                           (make-item (list (make-bullets '())))
                                           (make-item '()))))
              (make-bullets (list (make-item (list "This" "Is" "a" "Sentence"))
                                  (make-item (list "is" "a" "bad" "word")) ;; "is a bad word" indeed...
                                  (make-item (list (make-bullets '())))
                                  (make-item '()))))
;; Other tests are left as an exercise for the reader.

;; This function will follow the enum-temp, because it takes an enumeration as input.
(define (cleanse enum)
  (cond [(bullets? enum) (make-bullets (cleanse-loi (bullets-loi enum)))] ;; We'll cleanse the list of items, and put them pack in the struct
        [(points? enum) (make-points (cleanse-loi (points-loi enum)))]))  ;; Same as above


;; cleanse-loi : [List-of Item] -> [List-of Item]
;; Remove foul language from the list of items.

;; Tests are left as an exercise for the reader.

;; This function follows the loi-temp, because it takes a List of Items as input.
(define (cleanse-loi loi)
  (cond [(empty? loi) '()] ; If there are no items, we don't need to cleanse anything!
        [(cons? loi) (cons (cleanse-item (first loi))  ;; Otherwise cleanse the item, put it back in the list
                           (cleanse-loi (rest loi)))]))   ;; and cleans the rest of the list

;; cleanse-item : Item -> Item
;; Remove foul language from the item.

;; Tests are left as an exercise for the reader.

;; This function follows the item-temp, because it takes an Item as an input.
(define (cleanse-item i)
  (make-item (cleanse-low (item-low i))))


;; cleanse-low : [List-of Word] -> [List-of Word]
;; Remove foul language from a list of words.

;; Tests are left as an exercsie for the reader.

;; This function follows the list template, because it takes a List of Words as input.
(define (cleanse-low low)
  (cond [(empty? low) '()] ;; Easy case, nothing to cleanse so nothing to do.
        [(cons? low) (if (bad-word? (first low))  ;; Here we need to check whether a word is worth keeping or not, because a word could be a "bad word"
                         (cleanse-low (rest low)) ;; If it is a bad word, don't include it and go through the rest of the list
                         (cons (cleanse-word (first low)) (cleanse-low (rest low))))])) ;; Otherwise cleanse the word, include it and continue for the rest of the list.


;; bad-word? : Word -> Boolean
;; Is the given word a bad word worthy of censure?

(check-expect (bad-word? "Hello, nice to meet you") #false)
(check-expect (bad-word? "@#$%") #true)
;; Other tests where the word is an enumeration are left as an exercise for the reader.

;; We can write bad-word? using the word-template like this
(define (bad-word? w)
  (cond [(string? w) (string=? w "@#$%")]
        [else #false]))

;; But bad-word? can then be simplified to this definition:
(define (bad-word? w)
  (and (string? w) (string=? w "@#$%")))


;; cleanse-word : Word -> Word
;; Remove foul language from a Word.

(check-expect (cleanse-word "hello") "hello")
(check-expect (cleanse-word "@#$%") "") ;; This is one way of removing foul language from a String, remove the letters!
;; More tests are left as an exercise for the reader.

(define (cleanse-word w)
  (cond [(string? w) (if (bad-word? w) "" w)] ; Censor it if the string is a bad word, otherwise don't change it.
        [else (cleanse w)])) ;; If it is not a String, it is an enumeration!



