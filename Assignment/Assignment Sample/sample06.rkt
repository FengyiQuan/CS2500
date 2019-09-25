;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample06) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require neu-fall18)

#|--- DATA DEFINITIONS ---|#
;; A MusicPlayer is a (make-player String String FeedbackString)
(define-struct player [song1 song2 feedback])
;; - where song1 is the current song being played
;; - song2 is the next song to play
;; - and feedback is the feedback string to display to the user

;; A FeedbackString is one of:
;; - "" (represents no song played yet)
;; - "dislike"
;; - "like"
;; - "none"

#|--- CONSTANTS ---|#

(define BAD-FB DONT)
(define GOOD-FB LIKE)
(define NO-FB DONE)
(define NO-FB-TEXT "No feedback provided on the last song.")

(define SONG1PATH "../Project/OurSong.mp3")
(define SONG2PATH "../Project/ZootSuitRiot.mp3")
(define SONG1_BYTES (file-as-bytes SONG1PATH))
(define SONG2_BYTES (file-as-bytes SONG2PATH))

(define INITIAL-STATE (make-player SONG1_BYTES SONG2_BYTES ""))
(define EXAMPLE-PLAYER (make-player "bytes" "more bytes" NO-FB))
(define PLAYER2 (make-player SONG2_BYTES SONG1_BYTES BAD-FB))
(define PLAYER3 (make-player SONG1_BYTES SONG2_BYTES GOOD-FB))

(define INSTRUCTIONS "Click space to start playing a song.")
(define TEXT-SIZE 20)
(define INSTRUCTIONS-IMAGE (text INSTRUCTIONS TEXT-SIZE "black"))

#|--- TEMPLATES ---|#
;; music-player-template : MusicPlayer -> ???
(define (music-player-template mp)
  (... (player-song1 mp) ... (player-song2 mp) ... (player-feedback mp) ...))

;; feedback-template : FeedbackString -> ???
(define (feedback-template fs)
  (cond [(string=? fs "") ...]
        [(string=? fs BAD-FB) ...]
        [(string=? fs GOOD-FB) ...]
        [(string=? fs NO-FB) ...]))

#|--- BIG BANG ---|#
;; start-player : MusicPlayer -> String
;; Start the music player (produces most recent feedback when closed)
(define (start-player initial-player)
  (player-feedback
   (big-bang initial-player
     [to-draw draw-mp]
     [on-key if-space-play])))

#|--- TO-DRAW ---|#
;; draw-mp : MusicPlayer -> Image
;; Draw the current state of the world
(check-expect (draw-mp INITIAL-STATE)
              (above INSTRUCTIONS-IMAGE (draw-feedback "")))
(check-expect (draw-mp EXAMPLE-PLAYER)
              (above INSTRUCTIONS-IMAGE (draw-feedback NO-FB)))
(define (draw-mp aplayer)
  (above INSTRUCTIONS-IMAGE
         (draw-feedback (player-feedback aplayer))))

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

#|--- ON-KEY ---|#
;; if-space-play : MusicPlayer KeyEvent -> MusicPlayer
;; If the user presses the space bar play the next song
(check-expect (if-space-play INITIAL-STATE "x") INITIAL-STATE)
;; CANNOT TEST THE OTHER CASE SINCE IT INVOLVES USER FEEDBACK
(define (if-space-play aplayer akey)
  (if (key=? akey " ")
      (next-song aplayer (play-sound (player-song1 aplayer)))
      aplayer))

;; next-song : MusicPlayer FeedbackString -> MusicPlayer
;; Swap the songs and enter the feedback
(check-expect (next-song INITIAL-STATE BAD-FB) PLAYER2)
(check-expect (next-song PLAYER2 GOOD-FB) PLAYER3)
(define (next-song aplayer next-feedback)
  (make-player (player-song2 aplayer) (player-song1 aplayer) next-feedback))