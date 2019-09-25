;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample08) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Exercise 1

;; A Video is a (make-video String Number)
(define-struct video [name runtime])
;; and represents a video's name and runtime in seconds

(define MY-HOME-VIDEO (make-video "Puppy Chases Tail" 300))
(define MEME (make-video "just do what i do when i have problems." 4))

;; video-temp : Video -> ?
(define (video-temp v)
  (... (video-name v) (video-runtime v)))

;; Exercise 2

;; A Playlist is a (make-playlist String LoV)
(define-struct playlist [name videos])
;; and represent's a playlist's name and the videos it contains

;; An LoV (List of Video) is one of:
;; - '()
;; - (cons Video LoV)

(define LOV-0 '())
(define LOV-1 (cons MEME LOV-0))
(define LOV-2 (cons MY-HOME-VIDEO LOV-1))

(define PLAYLIST (make-playlist "Cheer Me Up" LOV-2))

;; playlist-temp : Playlist -> ?
(define (playlist-temp p)
  (... (playlist-name p) (lov-temp (playlist-videos p))))

;; lov-temp : LoV -> ?
(define (lov-temp lov)
  (cond [(empty? lov) ...]
        [(cons? lov) (... (video-temp (first lov))
                          (lov-temp (rest lov)))]))

;; Exercise 3

;; impatient : Playlist -> Playlist
;; Return a playlist with videos that are 90 seconds or less
(check-expect (impatient PLAYLIST) (make-playlist "Cheer Me Up" LOV-1))
(define (impatient p)
  (make-playlist (playlist-name p) (short-videos (playlist-videos p))))

;; short-videos : LoV -> LoV
;; Keep videos that are 90 seconds or less
(check-expect (short-videos LOV-0) LOV-0)
(check-expect (short-videos LOV-2) LOV-1)
(define (short-videos lov)
  (cond [(empty? lov) '()]
        [(cons? lov)
         (if (<= (video-runtime (first lov)) 90)
             (cons (first lov) (short-videos (rest lov)))
             (short-videos (rest lov)))]))

;; Exercise 4

;; too-long? : Playlist -> Boolean
;; Is this playlist > 10 min?
(check-expect (too-long? PLAYLIST) #f)
(check-expect (too-long? (make-playlist
                          "My video a lot"
                          (cons MY-HOME-VIDEO (cons MY-HOME-VIDEO (cons MY-HOME-VIDEO '())))))
              #t)
(define (too-long? p)
  (> (total-runtime (playlist-videos p)) (* 60 10)))

;; total-runtime : LoV -> Number
;; Total length of the list of videos
(check-expect (total-runtime LOV-0) 0)
(check-expect (total-runtime LOV-2) 304)
(define (total-runtime lov)
  (cond [(empty? lov) 0]
        [(cons? lov)
         (+ (video-runtime (first lov)) (total-runtime (rest lov)))]))

;; Exercise 5

;; A LoP (List of PlayLists) is one of:
;; - '()
;; - (cons Playlist LoP)

(define LOP-0 '())
(define LOP-1 (cons PLAYLIST LOP-0))
(define LOP-2 (cons PLAYLIST LOP-1))

;; lop-temp : LoP -> ?
(define (lop-temp lop)
  (cond [(empty? lop) ...]
        [(cons? lop) (... (playlist-temp (first playlist))
                          (lop-temp (rest playlist)))]))

;; playlists-runtime : LoP -> Number
;; Total runtime of playlists in minutes
(check-expect (playlists-runtime LOP-0) 0)
(check-expect (playlists-runtime LOP-2) 608/60)
(define (playlists-runtime lop)
  (cond [(empty? lop) 0]
        [(cons? lop) (+ (playlist-runtime (first lop))
                        (playlists-runtime (rest lop)))]))

;; playlist-runtime : Playlist -> Number
;; Length of playlist in minutes
(check-expect (playlist-runtime PLAYLIST) 304/60)
(define (playlist-runtime p)
  (/ (total-runtime (playlist-videos p)) 60))