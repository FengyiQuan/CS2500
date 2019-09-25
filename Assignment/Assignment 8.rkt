;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 8|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 1
(define-struct video [title runtime])
;; A Video is a (make-video String PositiveNumber)
;; where title represents the title of the video 
;; runtime represents the runtime of the video in seconds
;; Examples:
(define VIDEO-1 (make-video "abc" 70))
(define VIDEO-2 (make-video "hodi" 200))
(define VIDEO-3 (make-video "hello" 80))
#;
(define (video-temp v)
  (... (video-title v) ...
       (video-runtime v) ...))

; Exercise 2
;; A Lov (List of Video) is one of:
;; - '()
;; - (cons Video Lov)
;; represents a list of videos
;; Examples:
(define LoV-0 '())
(define LoV-1 (cons VIDEO-1'()))
(define LoV-2 (cons VIDEO-2 (cons VIDEO-1 '())))
(define LoV-3 (cons VIDEO-3 (cons VIDEO-2 (cons VIDEO-1 '()))))
(define LoV-4 (cons VIDEO-2 (cons VIDEO-2 (cons VIDEO-2 (cons VIDEO-3 '())))))
#;
(define (lov-temp lov)
  (cond
    [(empty? lov) ...]
    [(cons? lov) ... (video-temp (first lov)) ...
                 ... (video-temp (lov-temp (rest lov))) ...]))

(define-struct playlist [title lov])
;; A Playlist is a (make-playlist String LoV)
;; where title represents the title of the playlist itself
;; LoV (List of Video) represents a list of videos [title runtim]
;; Examples:
(define PL-0 (make-playlist "empty" LoV-0))
(define PL-1 (make-playlist "favorite" LoV-1))
(define PL-2 (make-playlist "double" LoV-2))
(define PL-3 (make-playlist "school" LoV-3))
(define PL-4 (make-playlist "long" LoV-4))
#;
(define (playlist-temp pl)
  (... (playlist-title pl) ...
       (lov-temp (playlist-lov pl)) ...))

; Exercise 3
;; impatient : Playlist -> Playlist
;; takes a Playlist, and returns a Playlist with the same title
;; but only containing the Videos that are 90 seconds or less
(check-expect (impatient PL-0) PL-0)
(check-expect (impatient PL-1) PL-1)
(check-expect (impatient PL-2) (make-playlist "double" (cons VIDEO-1 '())))
(check-expect (impatient PL-3) (make-playlist "school" (cons VIDEO-3 (cons VIDEO-1 '()))))
(define (impatient pl)
  (make-playlist (playlist-title pl) (less-than-90 (playlist-lov pl))))

;; less-than-90 : LoV -> LoV
;; make a List of Video that only contains video whose runtime is 90 seconds or less
(check-expect (less-than-90 LoV-0) LoV-0)
(check-expect (less-than-90 LoV-1) LoV-1)
(check-expect (less-than-90 LoV-2) LoV-1)
(check-expect (less-than-90 LoV-3) (cons VIDEO-3 (cons VIDEO-1 '())))
(define (less-than-90 lov)
  (cond
    [(empty? lov) '()]
    [(cons? lov) (if (<= (video-runtime (first lov)) 90)
                     (cons (first lov)
                           (less-than-90 (rest lov)))
                     (less-than-90 (rest lov)))]))

; Exercise 4
;; too-long? : Playlist -> Boolean
;; accepts a Playlist and determines if the total runtime of
;; all videos in the Playlist is more than 10 minutes (600 seconds)
(check-expect (too-long? PL-0) #false)
(check-expect (too-long? PL-1) #false)
(check-expect (too-long? PL-2) #false)
(check-expect (too-long? PL-3) #false)
(check-expect (too-long? PL-4) #true)
(define (too-long? pl)
  (> (total-time (playlist-lov pl)) 600))

;; total-time : LoV -> Number
;; calculate the total runtime of all videos in the list
(check-expect (total-time LoV-0) 0)
(check-expect (total-time LoV-1) 70)
(check-expect (total-time LoV-2) 270)
(check-expect (total-time LoV-3) 350)
(check-expect (total-time LoV-4) 680)
(define (total-time lov)
  (cond
    [(empty? lov) 0]
    [(cons? lov) (+ (video-runtime (first lov))
                    (total-time (rest lov)))]))

;; Exercise 5
;; A LoP (list of Playlists) is one of:
;; - '()
;; - (cons Playlist LoP)
;; represents a list of Playlists
;; Examples:
(define LoP-0 '())
(define LoP-1 (cons PL-1 LoP-0))
(define LoP-2 (cons PL-2 LoP-1))
(define LoP-3 (cons PL-3 LoP-2))
#;
(define (lop-temp lop)
  (cond
    [(empty? lop) ...]
    [(cons? lop) ... (playlist-temp (first lop)) ...
                 ... (lop-temp (rest lop)) ...]))

;; playlists-runtime : ListofPlaylists -> PostiveNumber
;; determines the total runtime in minutes of a list of Playlists
(check-expect (playlists-runtime LoP-0) 0)
(check-expect (playlists-runtime LoP-1) 70/60)
(check-expect (playlists-runtime LoP-2) 340/60)
(check-expect (playlists-runtime LoP-3) 11.5)
(define (playlists-runtime lop)
  (/ (total-runtime lop) 60))

;; total-runtime : ListofPlaylists -> PostiveNumber
;; calculate the total runtime of a ListofPlaylists
(check-expect (total-runtime LoP-0) 0)
(check-expect (total-runtime LoP-1) 70)
(check-expect (total-runtime LoP-2) 340)
(check-expect (total-runtime LoP-3) 690)
(define (total-runtime lop)
  (cond
    [(empty? lop) 0]
    [(cons? lop) (+ (total-time (playlist-lov (first lop)))
                    (total-runtime (rest lop)))]))