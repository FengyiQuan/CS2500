;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample11) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 1

;; square-evens : [List-of Number] -> [List-of Number]
;; Square the even numbers
(check-expect (square-evens (list 0 1 2 3 4)) (list 0 4 16))
(define (square-evens lon)
  (map sqr (filter even? lon)))

;; Exercise 2

;; square-evens/2 : [List-of Number] -> [List-of Number]
;; Square the even numbers
(check-expect (square-evens/2 (list 0 1 2 3 4)) (list 0 4 16))
(define (square-evens/2 lon)
  (local [;; square-if-even : Number [List-of Number] -> [List-of Number]
          ;; If the numer is even, square and attach it, otherwise drop it
          (define (square-if-even n lon)
            (if (even? n) (cons (sqr n) lon) lon))]
  (foldr square-if-even '() lon)))

;; Exercise 3

;; square-evens/3 : [List-of Number] -> [List-of Number]
;; Square the even numbers
(check-expect (square-evens/3 (list 0 1 2 3 4)) (list 0 4 16))
(define (square-evens/3 lon)
  (map-filter sqr even? lon))

;; map-filter : [X -> Y] [X -> Boolean] [List-of X] -> [List-of Y]
;; Map over the elements that pass p with f
(check-expect (map-filter string->number string? (list "1" 2 "3")) (list 1 3))
(define (map-filter f p lox)
  (local [;; apply-f-if-p : X [List-of Y] -> [List-of Y]
          ;; If x passes p, apply f and attach it, otherwise drop it
          (define (apply-f-if-p x loy)
            (if (p x) (cons (f x) loy) loy))]
  (foldr apply-f-if-p '() lox)))

;; Exercise 4

;; A Video is a (make-video String Number)
(define-struct video [name runtime])
;; and represents a video's name and runtime in seconds

(define MY-HOME-VIDEO (make-video "Puppy Chases Tail" 300))
(define MEME (make-video "just do what i do when i have problems." 4))

;; A Playlist is a (make-playlist String LoV)
(define-struct playlist [name videos])
;; and represent's a playlist's name and the videos it contains

(define PLAYLIST (make-playlist "Cheer Me Up" (list MY-HOME-VIDEO MEME)))

;; combine-playlists : String [List-of Playlist] -> Playlist
;; Make a new playlist which combines the videos of past ones and
;; uses name for the new name
(check-expect (combine-playlists "" (list PLAYLIST PLAYLIST))
              (make-playlist "" (list MY-HOME-VIDEO MEME MY-HOME-VIDEO MEME)))
(define (combine-playlists name playlists)
  (make-playlist name (combine-videos playlists)))

;; combine-videos : [List-of Playlist] -> [List-of Video]
;; Combine the videos in the playlists
(check-expect (combine-videos (list PLAYLIST PLAYLIST))
              (list MY-HOME-VIDEO MEME MY-HOME-VIDEO MEME))
(define (combine-videos lop)
  (local [;; add-videos : Playlist [List-of Video] -> [List-of Video]
          ;; Add videos in p to lov
          (define (add-videos p lov)
            (append (playlist-videos p) lov))]
    (foldr add-videos '() lop)))

;; Exerice 5

;; scalar-matrix : Nat Number -> [List-of [List-of Number]]
;; Return the scalar matrix of size n with scaling factor k
(check-expect (scalar-matrix 0 2) '())
(check-expect (scalar-matrix 3 2)
              (list (list 2 0 0)
                    (list 0 2 0)
                    (list 0 0 2)))
(define (scalar-matrix n k)
  (local [;; row : Nat -> [List-of Number]
          ;; The ith row
          (define (row i)
            (local [;; row-entry : Number -> Number
                    ;; The jth entry in row i
                    (define (row-entry j)
                      (if (= i j) k 0))]
              (build-list n row-entry)))]
    (build-list n row)))

;; Exercise 6

;; into : [List-of [List-of Nat]] Nat -> Nat
;; Follow the indices all the way down, where n is the index for the first list
(check-expect (into '() 3) 3)
(check-expect (into (list (list 3 5) (list 100 101 102 103 104 2) (list 1 1 0) (list 50 60)) 1) 50)
(check-error (into (list (list 0)) 1))
(define (into lolon n)
  (foldl list-ref n lolon))