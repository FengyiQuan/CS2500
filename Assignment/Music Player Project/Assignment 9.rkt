;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 9|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require neu-fall18)

;; Exercise 2
(define-struct song [title artist length album byte-string])
;; A song is a (make-song String String Number String)
;; - where the title is the song's title
;; - the artist is the song's artist
;; - the length is the length of the song (in seconds)
;; - and album is the song's album
;; - byte-string is a byte string of this song
;; Examples:
(define SONG-1 (make-song "Light" "Rocket Gilrs" 120 "Zhuang" #"bytes"))
;; song-temp : Song -> ???
(define (song-temp s)
  (... (song-title s) ...
       (song-artist s) ...
       (song-length s) ...
       (song-album s) ...))

;; Status is one of:
;; #false
;; #true
;; Song
;; Examples:
(define STATUS-F #false) ; just started the program
(define STATUS-T #true) ; requested a song but have not yet received one
(define STATUS-S SONG-1) ; have received a song from the server
;; status-temp : Status -> ???
(define (status-temp s)
  (cond [(and (boolean? s) (boolean=? #false s)) ...]
        [(and (boolean? s) (boolean=? #true s)) ...]
        [(song? s) (song-temp s)]))

;; A FeedbackString is one of:
;; - "" (represents no song played yet)
;; - "dislike"
;; - "like"
;; - "none"
;; Examples:
(define BAD-FB DONT)
(define GOOD-FB LIKE)
(define NO-FB DONE)
;; feedback-template : FeedbackString -> ???
(define (feedback-template fs)
  (cond [(string=? fs "") ...]
        [(string=? fs BAD-FB) ...]
        [(string=? fs GOOD-FB) ...]
        [(string=? fs NO-FB) ...]))

(define-struct musicplayer [status feedback])
;; A musicplayer is a (make-musicplayer Status FeedbackString)
;; where status represents the status of the program
;; feedback is the feedback string to display to the user
;; Examples:
(define MUSICPLAYER-1 (make-musicplayer STATUS-F ""))
(define MUSICPLAYER-2 (make-musicplayer STATUS-T BAD-FB))
(define MUSICPLAYER-3 (make-musicplayer STATUS-S GOOD-FB))
;; musicplayer-temp : Musicplayer -> ???
(define (musicplayer-temp aplayer)
  ( ... (status-temp (musicplayer-status aplayer)) ...
        (feedback-template (musicplayer-feedback aplayer)) ...))

;;|--- CONSTANTS ---|#
(define NO-FB-TEXT "No feedback provided on the last song.")
(define INSTRUCTIONS "Click space to start playing a song.")
(define TEXT-SIZE 20)
(define INSTRUCTIONS-IMAGE (text INSTRUCTIONS TEXT-SIZE "black"))

;; Exercise 3
#|--- BIG BANG ---|#
;; start-player : MusicPlayer -> String
;; Start the music player (produces most recent feedback when closed)
(define (start-player initial-player)
  (musicplayer-feedback
   (big-bang initial-player
     [to-draw draw-mp]
     [on-key if-space-play]
     [on-tick send-message]
     [on-receive receive-message]
     [register "dictionary.ccs.neu.edu"]
     [port 10001])))

#|--- TO-DRAW ---|#
;; draw-mp : MusicPlayer -> Image
;; Draw the current state of the world
(check-expect (draw-mp MUSICPLAYER-1)
              (above INSTRUCTIONS-IMAGE
                     (draw-feedback "")
                     (text "" TEXT-SIZE "black")
                     (text "" TEXT-SIZE "black")))
(check-expect (draw-mp MUSICPLAYER-2)
              (above INSTRUCTIONS-IMAGE
                     (draw-feedback BAD-FB)
                     (text "" TEXT-SIZE "black")
                     (text "" TEXT-SIZE "black")))
(define (draw-mp aplayer)
  (above INSTRUCTIONS-IMAGE
         (draw-feedback (musicplayer-feedback aplayer))
         (draw-title (musicplayer-status aplayer))
         (draw-artist (musicplayer-status aplayer))))

;; draw-feedback : FeedbackString -> Image
;; Draw the feedback in an appropriate color (red for bad, green for good)
(check-expect (draw-feedback "") (text "No song played yet." TEXT-SIZE "gray"))
(check-expect (draw-feedback BAD-FB) (text BAD-FB TEXT-SIZE "red"))
(check-expect (draw-feedback GOOD-FB) (text GOOD-FB TEXT-SIZE "green"))
(check-expect (draw-feedback NO-FB) (text NO-FB-TEXT TEXT-SIZE "gray"))
(define (draw-feedback fb)
  (cond [(string=? fb "") (text "No song played yet." TEXT-SIZE "gray")]
        [(string=? fb BAD-FB) (text BAD-FB TEXT-SIZE "red")]
        [(string=? fb GOOD-FB) (text GOOD-FB TEXT-SIZE "green")]
        [(string=? fb NO-FB) (text NO-FB-TEXT TEXT-SIZE "gray")]))

;; draw-title : Status -> Image
;; Draw the title of song playing currently on canvas
(check-expect (draw-title #true) (text "" TEXT-SIZE "black"))
(check-expect (draw-title #false) (text "" TEXT-SIZE "black"))
(check-expect (draw-title SONG-1) (text "Light" TEXT-SIZE "black"))
(define (draw-title s)
  (cond [(and (boolean? s) (boolean=? #false s)) (text "" TEXT-SIZE "black")]
        [(and (boolean? s) (boolean=? #true s)) (text "" TEXT-SIZE "black")]
        [(song? s) (text (song-title s) TEXT-SIZE "black")]))

;; draw-artist : Status -> Image
;; Draw the artist of song playing currently on canvas
(check-expect (draw-artist #true) (text "" TEXT-SIZE "black"))
(check-expect (draw-artist #false) (text "" TEXT-SIZE "black"))
(check-expect (draw-artist SONG-1) (text "Rocket Gilrs" TEXT-SIZE "black"))
(define (draw-artist s)
  (cond [(and (boolean? s) (boolean=? #false s)) (text "" TEXT-SIZE "black")]
        [(and (boolean? s) (boolean=? #true s)) (text "" TEXT-SIZE "black")]
        [(song? s) (text (song-artist s) TEXT-SIZE "black")]))

#|--- ON-KEY ---|#
;; if-space-play : MusicPlayer KeyEvent -> MusicPlayer
;; If the user presses the space bar play the next song
(check-expect (if-space-play MUSICPLAYER-1 "x") MUSICPLAYER-1)
;; CANNOT TEST THE OTHER CASE SINCE IT INVOLVES USER FEEDBACK
(define (if-space-play aplayer akey)
  (if (and (key=? akey " ") (song? (musicplayer-status aplayer)))
      (make-musicplayer #false (play-sound (song-byte-string (musicplayer-status aplayer))))
      aplayer))

#|--- ON-TICK ---|#
;; send-message : MusicPlayer -> MusicPlayer
;; if the program just started, request a song from server
(check-expect (send-message MUSICPLAYER-1) (make-package (make-musicplayer #true "") "next"))
(check-expect (send-message MUSICPLAYER-2) MUSICPLAYER-2)
(check-expect (send-message MUSICPLAYER-3) MUSICPLAYER-3)
(define (send-message musicplayer)
  (cond [(and (boolean? (musicplayer-status musicplayer))
              (boolean=? #false (musicplayer-status musicplayer)))
         (make-package (make-musicplayer #true (musicplayer-feedback musicplayer)) "next")]
        [else musicplayer]))

; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
 
; An ErrorMsg is a (list "ERROR" String)
; where the second string is the message from the server about what went wrong
#;
(define (errormsg-temp emg)
  (... (first emg) ...
       (second emg) ...))
; A SongMsg is a (list "SONG" Metadata String)
; - where the metadata is information about the given song
; - and the second the String is the actual byte-string of music
;; Examples:
;(define SONGMSG-1 (list METADATA-1 
#;
(define (songmsg-temp smg)
  (... (first smg) ...
       (mtdt-temp (second smg)) ...
       (third smg) ...))
; A Metadata is a (list String String Number String)
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album
;; Examples:
(define METADATA-1 (list "Light" "Rocket Gilrs" 120 "Zhuang"))
#;
(define (mtdt-temp m)
  (... (first m) ...
       (second m) ...
       (third m) ...
       (fourth m) ...))
;; servermsg-temp : ServerMsg -> ???
(define (servermsg-temp smg)
  (cond
    [(string=? "ERROR" (first smg)) ...]
    [(string=? "SONG" (first smg)) ...]))

#|--- ON-RECEIVE ---|#
;; receive-message : MusicPlayer ServerMsg -> PlayerResult
;; returns a PlayerResult
(check-expect (receive-message MUSICPLAYER-1
                               (list "SONG" (list "Light" "Rocket Gilrs" 120 "Zhuang") #"bytes"))
              (make-musicplayer (make-song "Light" "Rocket Gilrs" 120 "Zhuang" #"bytes") ""))
(check-expect (receive-message MUSICPLAYER-2
                               (list "ERROR" (list  "Light" "Rocket Gilrs" 120 "Zhuang") #"bytes"))
              (make-package MUSICPLAYER-2 "next"))
(define (receive-message aplayer smg)
  (cond
    [(string=? "ERROR" (first smg)) (make-package aplayer "next")]
    [(string=? "SONG" (first smg)) (make-musicplayer
                                    (make-song (first (second smg))
                                               (second (second smg))
                                               (third (second smg))
                                               (fourth (second smg))
                                               (third smg))
                                    (musicplayer-feedback aplayer))]))