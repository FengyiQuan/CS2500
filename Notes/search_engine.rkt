;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname search_engine) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Simple Search Engine

;; A Corpus is [List-of Doc]
;; A Doc is (make-doc String [List-of Term])
;; A Query is [List-of Term]
;; A Term is String
;; A Score is (make-score Doc Number)|

(define-struct doc [id content])
(define-struct score [doc score])

(define D1 (make-doc "D1" (list "common" "blackberry" "bugs")))
(define D2 (make-doc "D2" (list "fix" "scratch" "in" "blackberry" "screen")))
(define D3 (make-doc "D3" (list "great" "recipe" "blackberry" "pie" "from" "scratch" "recipe")))
(define D4 (make-doc "D4" (list "screen" "HD" "black" "friday" "deals")))

(define C1 (list D1 D2 D3 D4))
(define C2 (list D1))
(define Q1 (list "blackberry" "screen"))

;; tf/term_doc: Term Doc -> Nat
;; computes the number of occurrences of the given term in the given document (term frequency)
(check-expect (tf/term_doc "bugs" D1) 1)
(check-expect (tf/term_doc "bugs" D2) 0)
(check-expect (tf/term_doc "recipe" D3) 2)

(define (tf/term_doc t d)
  #;(foldr (λ (term tf) (if (string=? term t) (add1 tf) tf))  0 (doc-content d))
  (length (filter (λ (term) (string=? term t))  (doc-content d))))

;; df/term_corpus: Term Corpus -> Nat
;; computes the number of documents in which the term appears 9at least once)
;; document frequency
(check-expect (df/term_corpus "blackberry" C1) 3)
(check-expect (df/term_corpus "batman" C1) 0)
(check-expect (df/term_corpus "HD" C1) 1)

(define (df/term_corpus t c)
  (foldr (λ (doc df) (if (member? t (doc-content doc)) (add1 df) df))  0 c))


;; tf.idf: Query Corpus -> [List-of Score]
;; computes the tf.idf scores for each documet in the given corpus in response to the given query

(check-within (tf.idf Q1 C2) (list (make-score D1 0.30685)) 0.1)
(check-expect (tf.idf Q1 '()) '())

(define (tf.idf q c)
  (map (λ (d) (make-score d
                          (foldr + 0
                                 (map (λ (t) (* (tf/term_doc t d)
                                                (add1 (log (/ (length c) (add1 (df/term_corpus t c)))))))
                                                 q))))
       c))

;; * Note: add1 to avoid division by zero

;; rank: Query Corpus -> [List-of Score]
;; ranks the documents in the corpus based on their relevance to the given (starting with the most relevant one)
(define (rank q c)
  (quicksort (tf.idf q c)
             (λ (s1 s2) (> (score-score s1) (score-score s2)))))

;; feeling-lucky: Query Corpus -> Score
;; simulates Google's feeling lucky
(define (feeling-lucky q c)
  (first (rank q c)))



(rank Q1 C1)