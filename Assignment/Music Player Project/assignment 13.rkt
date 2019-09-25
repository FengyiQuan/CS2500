;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |assignment 13|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require neu-fall18)
(require 2htdp/batch-io)

;; Exercise 1
;; Data definition:
#|------------- Song -------------|#
(define-struct song [id title artist byte-string])
;; A song is a (make-song Number String String Number String)
;; - where id is the song's id number
;; - the title is the song's title
;; - the artist is the song's artist
;; - byte-string is a byte string of this song
;; Examples:
(define SONG-1 (make-song 123456 "Light" "Rocket Girls" #"bytes"))
;; song-temp : Song -> ???
(define (song-temp s)
  (... (song-id s) ...
       (song-title s) ...
       (song-artist s) ...
       (song-length s) ...
       (song-byte-string s) ...))

#|------------- Status -------------|#
;; Status is one of:
;; #false
;; #true
;; Song
;; Examples:
(define STATUS-F #false) ; just started the program or we did not request a song from server
(define STATUS-T #true) ; requested a song but have not yet received one
(define STATUS-S SONG-1) ; have received a song from the server
;; status-temp : Status -> ???
(define (status-temp s)
  (cond [(and (boolean? s) (boolean=? #false s)) ...]
        [(and (boolean? s) (boolean=? #true s)) ...]
        [(song? s) (song-temp s)]))

#|------------- FeedbackString -------------|#
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

#|------------- History -------------|#
(define-struct 1song-history [id name plays])
;; A HO1S (History of one song) is a (make-1song-history Number String Nat)
;; where id number represents the id of a song
;; the name is the name of a song
;; the plays is the number of how many times this song has been played
(define HO1S-1 (make-1song-history 123456 "Light" 0))
(define HO1S-2 (make-1song-history 654321 "abc" 5))
#;
(define (ho1s-temp h1)
  (... (1song-history-id h1) ...
       (1song-history-name h1) ...
       (1song-history-plays h1) ...))
;; A History is a [List-of HO1S]
;; represents the all song's history
(define HISTORY-0 '())
(define HISTORY (list HO1S-1 HO1S-2))
#;
(define (history-tmep h)
  (cond
    [(empty? h) ...]
    [(cons? h) ... (first h) ...
               (history-tmep (rest h) ...)]))
#|------------- MusicPlayer -------------|#
(define-struct musicplayer [status feedback history])
;; A musicplayer is a (make-musicplayer Status FeedbackString History)
;; where status represents the status of the program
;; feedback is the feedback string to display to the user
;; history represents the hisotory of played music
(define MP-#f (make-musicplayer STATUS-F "" '()))
(define MP-#t (make-musicplayer STATUS-T BAD-FB '()))
(define MP-SONG (make-musicplayer STATUS-S GOOD-FB HISTORY))

#|------------- ServerMsg -------------|#
;; An ErrorMsg is a (list "ERROR" String)
;; where the second string is the message from the server about what went wrong
(define EMSG (list "ERROR" "WRONG"))
#;
(define (errormsg-temp emg)
  (... (first emg) ...
       (second emg) ...))
;; A SongMsg is a (list "SONG" Nat Metadata String)
;; - where the Nat is the song's unique ID#
;; - the Metadata is information about the song
;; - and the String is the actual byte-string of music
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
(define METADATA (list "Light" "Rocket Gilrs" 120 "No album"))
#;
(define (mtdt-temp m)
  (... (first m) ...
       (second m) ...
       (third m) ...
       (fourth m) ...))
; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
(define SMG-ERROR EMSG)
(define SMG-SONG (list "SONG" 123456 METADATA  #"bytes"))
#;
(define (servermsg-temp smg)
  (cond
    [(string=? "ERROR" (first smg)) ... (errormsg-temp smg) ...]
    [(string=? "SONG" (first smg)) ... (songmsg-temp smg) ...]))

;; constants:
(define FILEPATH "history.csv")
(define BG (empty-scene 1000 500))
(define NO-FB-TEXT "No feedback provided on the last song.")
(define INSTRUCTIONS "Click space to start playing a song.")
(define TEXT-SIZE 20)

#|------------- BIG BANG -------------|#
;; main/player : MusicPlayer -> MusicPlayer
;; creates a musicplayer that keeps track of song history 
(define (main/player _)
  (save-history FILEPATH
                (big-bang (read-history FILEPATH)
                  [to-draw draw-mp]
                  [on-key if-space-play]
                  [on-tick send-message]
                  [on-receive receive-message]
                  [register "dictionary.ccs.neu.edu"]
                  [port 10001])))

#|------------- Save & Read File -------------|#
;; save-history : String MusicPlayer -> String
;; Save the number of time that a song has been played to a file
(define (save-history fpath musicplayer)
  (write-file fpath (history->string (musicplayer-history musicplayer))))

;; history->string : History -> String
;; convert history to a string that can be written in a file
(check-expect (history->string HISTORY-0) "")
(check-expect (history->string HISTORY)
              (string-append "123456 ,Light ,0\n654321 ,abc ,5\n"))
(define (history->string h)
  (foldr combine-ho1s "" h))

;; combine-ho1s : HO1S String -> String
;; and append all string together
(check-expect (combine-ho1s HO1S-1 "") "123456 ,Light ,0\n")
(check-expect (combine-ho1s HO1S-2 "") "654321 ,abc ,5\n")
(define (combine-ho1s ho1s s)
  (string-append (ho1s->string ho1s) s))
              
;; ho1s->string: HO1S -> String
;; convert HO1s to a string 
(check-expect (ho1s->string HO1S-1) "123456 ,Light ,0\n")
(check-expect (ho1s->string HO1S-2) "654321 ,abc ,5\n")
(define (ho1s->string ho1s)
  (string-append (number->string (1song-history-id ho1s))
                 " ,"
                 (1song-history-name ho1s)
                 " ,"
                 (number->string (1song-history-plays ho1s))
                 "\n"))

;; read-history : String -> MusicPlayer
;; Produces MusicPlayer from the file or produce a new file if the file does not exist
(check-expect (read-history "a new history") MP-#f)
(define (read-history fpath)
  (if (file-exists? fpath)
      (make-musicplayer STATUS-F "" (lol->history (read-csv-file fpath)))
      (make-musicplayer STATUS-F "" '())))

;; lol->history : [List-of [List-of String]] -> History
;; convert file to history
(check-expect (lol->history '()) '())
(check-expect (lol->history (list (list "1" "Siesta" "1") (list "0" "Cheese" "1")))
              (list (make-1song-history 1 "Siesta" 1) (make-1song-history 0 "Cheese" 1)))
(define (lol->history lolos)
  (map onelist->ho1s lolos))

;; onelist->ho1s : [List-of String] -> HO1S
;; convert a list of three elements into a HO1S
(check-expect (onelist->ho1s (list "1" "Siesta" "1")) (make-1song-history 1 "Siesta" 1))
(define (onelist->ho1s l)
  (make-1song-history (string->number (first l))
                      (second l)
                      (string->number (third l))))

#|--------- TO-DRAW ---------|#
;; draw-mp : MusicPlayer -> Image
;; Draw the current state of the world
(check-expect (draw-mp MP-#f)
              (overlay (above (text INSTRUCTIONS TEXT-SIZE "black")
                              (text "No song played yet." TEXT-SIZE "gray")
                              (text "" TEXT-SIZE "black")
                              (text "" TEXT-SIZE "black")
                              (text "" TEXT-SIZE "black"))
                       BG))
(check-expect (draw-mp MP-#t)
              (overlay (above (text INSTRUCTIONS TEXT-SIZE "black")
                              (text BAD-FB TEXT-SIZE "red")
                              (text "" TEXT-SIZE "black")
                              (text "" TEXT-SIZE "black")
                              (text "" TEXT-SIZE "black"))
                       BG))
(define (draw-mp aplayer)
  (overlay (above (text INSTRUCTIONS TEXT-SIZE "black")
                  (draw-feedback (musicplayer-feedback aplayer))
                  (draw-title (musicplayer-status aplayer))
                  (draw-artist (musicplayer-status aplayer))
                  (draw-history (musicplayer-history aplayer)))
           BG))

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
(check-expect (draw-artist SONG-1) (text "Rocket Girls" TEXT-SIZE "black"))
(define (draw-artist s)
  (cond [(and (boolean? s) (boolean=? #false s)) (text "" TEXT-SIZE "black")]
        [(and (boolean? s) (boolean=? #true s)) (text "" TEXT-SIZE "black")]
        [(song? s) (text (song-artist s) TEXT-SIZE "black")]))

;; draw-history : History -> Image
;; Draw the history of song has been played on canvas
(check-expect (draw-history HISTORY-0) (text "" TEXT-SIZE "black"))
(check-expect (draw-history HISTORY) (above (text "123456 Light 0" TEXT-SIZE "black")
                                            (text "654321 abc 5" TEXT-SIZE "black")
                                            (text "" TEXT-SIZE "black")))
(define (draw-history h)
  (foldr ho1s->image (text "" TEXT-SIZE "black") h))

;; ho1s->image : HO1S Image -> Image
;; Draw the text of history of one song
(check-expect (ho1s->image HO1S-1 empty-image) (text "123456 Light 0" TEXT-SIZE "black"))
(define (ho1s->image h1 i)
  (above (text (read-ho1s h1) TEXT-SIZE "black")
         i))

;; read-ho1s : HO1S -> String
;; read a history of one song as a string
(check-expect (read-ho1s HO1S-1) "123456 Light 0")
(define (read-ho1s ho1s)
  (string-append (number->string (1song-history-id ho1s)) " "
                 (1song-history-name ho1s) " "
                 (number->string (1song-history-plays ho1s))))
#|--------- ON-KEY ---------|#
;; if-space-play : MusicPlayer KeyEvent -> MusicPlayer
;; If the user presses the space bar play the next song
(check-expect (if-space-play MP-#f "x") MP-#f)
(define (if-space-play aplayer akey)
  (local [;; change-history : History -> History
          ;; gets a new history after a new song played
          (define (change-history h)
            (local [;; add1-to-this-list : History -> History
                    ;; add 1 to the history
                    ;; Given HISTORY, (ho1s=HO1S-1),
                    ;; returns (list (make-1song-history 123456 "Light" 1) HO1S-2))     
                    (define (add1-to-this-list h)
                      (local [;; add1-1sh : HO1S -> HO1S
                              ;; add 1 to HO1S plays
                              ;; Given HO1S-1, (song-id="123456"),
                              ;; returns (make-1song-history 123456 "Light" 1)
                              (define (add1-1sh ho1s)
                                (if (= (1song-history-id ho1s) (song-id (musicplayer-status aplayer)))
                                    (make-1song-history (1song-history-id ho1s)
                                                        (1song-history-name ho1s)
                                                        (add1 (1song-history-plays ho1s)))
                                    ho1s))]
                        (map add1-1sh h)))
                    ;; build-new-list : History -> History
                    ;; add a new HO1S to the history
                    ;; Given HISTORY, (song-id="111", song-title="I'm done with this.")
                    ;; returns (cons (list "111" "I'm done with this." 1) HISTORY)
                    (define (build-new-list h)
                      (cons (make-1song-history (song-id (musicplayer-status aplayer))
                                                (song-title (musicplayer-status aplayer))
                                                1) h))]
              (if (compare-to-history? h aplayer)
                  (add1-to-this-list h)
                  (build-new-list h))))]
    (if (and (key=? akey " ") (song? (musicplayer-status aplayer)))
        (make-musicplayer #false (return-fb aplayer)
                          (change-history (musicplayer-history aplayer)))
        aplayer)))

;; compare-to-history? : History MusicPlayer -> Boolean
;; determine if the id of a song in history is the same as the id of song playing
(check-expect (compare-to-history? HISTORY MP-SONG) #t)
(check-expect (compare-to-history? HISTORY-0 MP-#f) #f)
(define (compare-to-history? h mp)
  (local [(define (compare-each-ho1s ho1s)
            (if (song? (musicplayer-status mp))
                (= (1song-history-id ho1s) (song-id (musicplayer-status mp)))
                #f))]
    (ormap compare-each-ho1s h)))

;; return-fb : MusicPlayer -> FeedbackString
;; returns a feedback of the given player
(define (return-fb mp)
  (play-sound (song-byte-string (musicplayer-status mp))))

#|--------- ON-TICK ---------|#
;; send-message : MusicPlayer -> MusicPlayer
;; if the program just started or status is #false, request a song from server
(check-expect (send-message MP-#f) (make-package (make-musicplayer #true "" '()) "next"))
(check-expect (send-message MP-#t) MP-#t)
(check-expect (send-message MP-SONG) MP-SONG)
(define (send-message musicplayer)
  (cond [(and (boolean? (musicplayer-status musicplayer))
              (boolean=? #false (musicplayer-status musicplayer)))
         (make-package (make-musicplayer #true (musicplayer-feedback musicplayer)
                                         (musicplayer-history musicplayer)) "next")]
        [else musicplayer]))

#|--------- ON-RECEIVE ---------|#
;; receive-message : MusicPlayer ServerMsg -> PlayerResult
;; deal with the package from server to receive a new song and play it
(check-expect (receive-message MP-#f
                               (list "SONG" 12345
                                     (list "Light" "Rocket Gilrs" 120 "Zhuang") #"bytes"))
              (make-musicplayer (make-song 12345 "Light" "Rocket Gilrs" #"bytes") "" '()))
(check-expect (receive-message MP-#t
                               (list "ERROR" 12345
                                     (list "Light" "Rocket Gilrs" 120 "Zhuang") #"bytes"))
              (make-package MP-#t "next"))
(define (receive-message aplayer smg)
  (cond
    [(string=? "ERROR" (first smg)) (make-package aplayer "next")]
    [(string=? "SONG" (first smg)) (make-musicplayer
                                    (make-song (second smg)
                                               (first (third smg))
                                               (second (third smg))
                                               (fourth smg))
                                    (musicplayer-feedback aplayer)
                                    (musicplayer-history aplayer))]))
