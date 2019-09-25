;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 6|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require neu-fall18)

; A FeedbackString is one of:
; - ""
; - "dislike"
; - "like"
; - "none"
; Interpretation: The feedback that the user gave to the last song played.  The string
; "none" represents that the user gave no feedback, and the string "" represents
; that no feedback has been received yet (i.e., we are playing the first song).
(define FEEDBACKSTRING-EMPTY "")
(define FEEDBACKSTRING-DISLIKE "dislike")
(define FEEDBACKSTRING-LIKE "like")
(define FEEDBACKSTRING-NONE "none")
#;
(define (feedbackstring-temp fs)
  (cond
    [(string=? fs FEEDBACKSTRING-EMPTY) ...]
    [(string=? fs FEEDBACKSTRING-DISLIKE) ...]
    [(string=? fs FEEDBACKSTRING-LIKE) ...]
    [(string=? fs FEEDBACKSTRING-NONE) ...]))

;; Constant:
(define TEXT-SIZE 30)
(define TEXT-COLOR "blue")
(define WIDTH 400)
(define HEIGHT 200)
(define BACKGROUND (rectangle WIDTH HEIGHT "outline" "black"))
(define SongName-1 "Light")
(define SongName-2 "RELAX")
(define BYTESTRING-1 (make-song-bytes SongName-1 (file-as-bytes "Light.mp3")))
(define BYTESTRING-2 (make-song-bytes SongName-2 (file-as-bytes "RELAX.mp3")))

(define-struct player [song1 song2 feedback])
; A MusicPlayer is a (make-player String String FeedbackString)
; Interpretation: The state of the music player
; - song1 is the bytes of the current song being played
; - song2 is the bytes of the next song to played
; - feedback is the feedback received from the user for the last song played
; Examples:
(define PLAYER-1 (make-player BYTESTRING-1 BYTESTRING-2 FEEDBACKSTRING-LIKE))
(define PLAYER-2 (make-player BYTESTRING-2 BYTESTRING-1 FEEDBACKSTRING-NONE))
#;
(define (musicplayer-temp mp)
  ... (player-song1 mp) ... (player-song2 mp) ... (player-feedback mp) ...)

;; Exercise 2
;; main/player : Player -> Feedback
;; plays two music repeatedly and gives feedback for the last music 
(define (main/player plr)
  (big-bang plr
    [to-draw feedback]
    [on-key play-song]))

;; feedback : Player -> Image
;; and draws feedback of the last song on the screen
(check-expect (feedback PLAYER-1) (above (text "Press space to play a song." TEXT-SIZE TEXT-COLOR)
                                         (text (player-feedback PLAYER-1)
                                               TEXT-SIZE TEXT-COLOR)))
(check-expect (feedback PLAYER-2) (above (text "Press space to play a song." TEXT-SIZE TEXT-COLOR)
                                         (text (player-feedback PLAYER-2)
                                               TEXT-SIZE TEXT-COLOR)))
(define (feedback plr)
  (above (text "Press space to play a song." TEXT-SIZE TEXT-COLOR)
         (text (player-feedback plr) TEXT-SIZE TEXT-COLOR)))

;; play-song : Player KeyEvent -> Player
;; plays song when pressing space, and plays the next song when pressing space again
(define (play-song plr ke)
  (cond
    [(and (key=? ke " ")(string=? "Light" (song-bytes-title (player-song1 plr))))
     (make-player BYTESTRING-2 BYTESTRING-1 (play-sound (player-song1 PLAYER-2)))]
    [(and (key=? ke " ")(string=? "RELAX" (song-bytes-title (player-song1  plr))))
     (make-player BYTESTRING-1 BYTESTRING-2 (play-sound (player-song1 PLAYER-1)))]))