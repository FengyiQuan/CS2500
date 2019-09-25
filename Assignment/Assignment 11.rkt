;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
;; Exercise 1
;; squares-of-even : [List-of Number] -> [List-of Number]
;; return the squares of the even numbers in the list
(check-expect (squares-of-even (list 1 2 3 4)) (list 4 16))
(check-expect (squares-of-even (list 0 1 3)) (list 0))
(check-expect (squares-of-even (list 1 3)) '())
(define (squares-of-even lon)
  (map sqr (filter even? lon)))

;; Exercise 2
;; squares-of-even.v.2 : [List-of Number] -> [List-of Number]
;; return the squares of the even numbers in the list
(check-expect (squares-of-even.v.2 (list 1 2 3 4)) (list 4 16))
(check-expect (squares-of-even.v.2 (list 0 1 3)) (list 0))
(check-expect (squares-of-even.v.2 (list 1 3)) '())
(define (squares-of-even.v.2 lon)
  (foldr cons-sqr-even '()  lon))

;; cons-sqr-even : Number [List-of Number] -> [List-of Number]
;; make a list of square of even numbers 
(define (cons-sqr-even n1 n2)
  (cond [(even? n1) (cons (sqr n1) n2)]
        [else n2]))

;; Exercise 3
;; map-filter : (X Y) [List-of X] [X -> Y] [X -> Boolean] -> Y
(define (map-filter lox func test)
  (local [;; filter-lox : X [List-of X] -> [List-of X]
          ;; returns a filtered list based on a given test
          (define (filter-lox  x lox)
            (if (test x)
                (cons (func x) lox)
                lox))]
    (foldr filter-lox '() lox)))

;; squares-of-even.v.3 : [List-of Number] -> [List-of Number]
;; return the squares of the even numbers in the list
(check-expect (squares-of-even.v.3 (list 1 2 3 4)) (list 4 16))
(check-expect (squares-of-even.v.3 (list 0 1 3)) (list 0))
(check-expect (squares-of-even.v.3 (list 1 3)) '())
(define (squares-of-even.v.3 lon)
  (map-filter lon sqr even?))

;; Exercise 4  
;; A Video is a (make-video String Number)
(define-struct video [name runtime])
;; and represents a video's name and runtime in seconds
(define MY-HOME-VIDEO (make-video "Puppy Chases Tail" 300))
(define MEME (make-video "just do what i do when i have problems." 4))
;; video-temp : Video -> ?
(define (video-temp v)
  (... (video-name v) (video-runtime v)))
;; List-of Video (LoV) is one of:
;; - '()
;; - (cons video LoV) 
(define LOV-0 '())
(define LOV-1 (cons MEME LOV-0))
(define LOV-2 (cons MY-HOME-VIDEO LOV-1))

;; A Playlist is a (make-playlist String LoV)
(define-struct playlist [name videos])
;; and represent's a playlist's name and the videos it contains
(define PLAYLIST-1 (make-playlist "Cheer Me Up" LOV-2))
(define PLAYLIST-2 (make-playlist "adsf" LOV-1))
;; playlist-temp : Playlist -> ?
(define (playlist-temp p)
  (... (playlist-name p) (lov-temp (playlist-videos p))))
;; lov-temp : LoV -> ?
(define (lov-temp lov)
  (cond [(empty? lov) ...]
        [(cons? lov) (... (video-temp (first lov))
                          (lov-temp (rest lov)))]))
;; List-of Playlist (LoP) is one of:
;; '()
;; (cons Playlist LoP)
(define LOP-1 (cons PLAYLIST-1 '()))
(define LOP-2 (cons PLAYLIST-1 (cons PLAYLIST-2 '())))

;; change-name : [List-of Playlist] string -> Playlist
;; given a list of playlists and a string, title, produces a new playlist with the given title
;; which contains all of the videos of the given playlists (in the order given).
(check-expect (change-name LOP-1 "ABC") (make-playlist "ABC" LOV-2))
(check-expect (change-name LOP-2 "great homework")
              (make-playlist "great homework" (list MY-HOME-VIDEO MEME MEME)))
(define (change-name lop title)
  (make-playlist title (all-videos-in-playlists lop)))

;; all-videos-in-playlists : [List-of Playlist] -> [List-of Video]
;; returns all Video in a list of Playlist
(check-expect (all-videos-in-playlists LOP-1) LOV-2)
(check-expect (all-videos-in-playlists LOP-2) (list MY-HOME-VIDEO MEME MEME))
(define (all-videos-in-playlists lop)
  (foldr append-playlist-videos '() lop))

;; append-playlist-videos : Playlist [List-of Video] -> [List-of Video]
;; make a list of all videos
(check-expect (append-playlist-videos PLAYLIST-1 LOV-0) LOV-2)
(check-expect (append-playlist-videos PLAYLIST-2 LOV-1) (list MEME MEME))
(define (append-playlist-videos p lov)
  (append (playlist-videos p) lov))

;; Exercise 5
;; scalar-matrix : PositiveInteger PositiveInteger -> [List-of [List-of Number]]
;; given a natural number n and a number k, outputs an nxn matrix (list of list of numbers)
;; where the diagonal (first element of the first row, second element of the second row...
;; nth element of the nth row) entries are k and the other elements are 0.
(check-expect (scalar-matrix 1 2) (list (list 2)))
(check-expect (scalar-matrix 2 3) (list (list 3 0) (list 0 3)))
(check-expect (scalar-matrix 3 4) (list (list 4 0 0) (list 0 4 0) (list 0 0 4)))
(define (scalar-matrix n k)
  (local [;; make-one-list : Number -> [List-of Number]
          ;; to make each list for matrix
          ;; Given x=1, (n=1, k=2), returns (list 1)
          ;; Given x=2, (n=3, k=4),nreturns (list 0 4 0)
          (define (make-one-list x)
            (local [;; pos : Nat -> Number
                    ;; determine if the index of the sub list is equal to the index of the main list
                    ;; Given y=1 (x=1, k=4), retuns 4
                    ;; Given y=2 (x=1, k=4) ,returns 0
                    (define (pos y)
                      (if (= y x) k 0))]
              (build-list n pos)))]
    (build-list n make-one-list)))

;; Exercise 6
;; into : [List-of [List-of Number]] Number -> Number
;; takes in a list of list of natural numbers, lolon, and a natural number, n. n will operate
;; as an index into the first entry in lolon, and the element at that location will operate as
;; an index into the second entry in lolon, etc. until all of lolon is exhausted.
(check-expect (into (list (list 3 5) (list 100 101 102 103 104 2) (list 1 1 0) (list 50 60)) 1) 50)
(check-expect (into (list (list 1 2) (list 2 3 4 5 6) (list 1 2 3 4)) 0) 4)
(define (into lolon n)
  (foldl list-ref n lolon))