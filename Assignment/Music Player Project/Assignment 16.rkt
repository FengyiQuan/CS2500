;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 16|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)
(require neu-fall18)
(require 2htdp/batch-io)

#|------------- Song -------------|#
(define-struct song [id title artist byte-string])
;; A song is a (make-song Number String String String)
;; - where id is the song's id number
;; - the title is the song's title
;; - the artist is the song's artist
;; - byte-string is a byte string of this song
(define SONG (make-song 123456 "Light" "Rocket Girls" #"bytes"))
#; (define (song-temp s)
     (... (song-id s) ...
          (song-title s) ...
          (song-artist s) ...
          (song-byte-string s) ...))

#|------------- ClientMsg -------------|#
; A ClientMsg is a Nat
; representing a request for a SongMsg with a specific ID#
(define CMSG 0)

#|------------- ServerMsg -------------|#
;; An ErrorMsg is a (list "ERROR" String)
;; where the second string is the message from the server about what went wrong
(define EMSG (list "ERROR" "WRONG"))
#; (define (errormsg-temp emg)
     (... (first emg) ...
          (second emg) ...))

; A Metadata is a (list String String Number String)
; - where the first String is the song's title
; - the second String is the song's artist
; - the Number is the length of the song (in seconds)
; - and the third String is the song's album
(define METADATA-1 (list "Light" "Rocket Gilrs" 120 "No album"))
(define METADATA-2 (list "REALX" "Jack Yong" 123 "album"))
#; (define (mtdt-temp m)
     (... (first m) ...
          (second m) ...
          (third m) ...
          (fourth m) ...))
;; A SongMsg is a (list "SONG" Nat Metadata String)
;; - where the Nat is the song's unique ID#
;; - the Metadata is information about the song
;; - and the String is the actual byte-string of music
(define SMSG (list "SONG" 123456 METADATA-1 #"bytes"))
#; (define (songmsg-temp smg)
     (... (first smg) ...
          (second smg) ...
          (mtdt-temp (third smg)) ...
          (fourth smg) ...))

; A IDMetaPair is a (list Nat Metadata)
; - where the nat is the id of the song
; - and the metadata contains all of the metadata of the song
(define IDPAIR-1 (list 123456 METADATA-1))
(define IDPAIR-2 (list 654321 METADATA-2))
#; (define (idpair-temp idp)
     (... (first idp) ...
          (mtdt-temp (second idp)) ...))
;; A List-of-IDP is a [List-of IDMetaPair]
(define LOIDP (list IDPAIR-1 IDPAIR-2))
#; (define (loidp-temp lodip)
     (cond
       [(empty? lodip) ...]
       [(cons? lodip) ... (idpair-temp (first lodip))
                      (loidp-temp (rest lodip)) ...]))
; A MetadataMsg is a (list "METADATA" [List-of IDMetaPair])
; where the list contains all of the metadata and ids of the songs available on the server
(define MMSG (list "METADATA" LOIDP))
#; (define (metadatamsg-temp mmg)
     (... (first mmg) ...
          (loidp-temp (second mmg)) ...))

; A ServerMsg is one of:
; - ErrorMsg
; - SongMsg
; - MetadataMsg
(define SMSG-1 EMSG)
(define SMSG-2 SMSG)
(define SMSG-3 MMSG)

#|------------- Status -------------|#
;; Status is one of:
;; "Waiting for song data"
;; "Choosing a song"
;; "Requesting song"
;; Song
;; Examples:
(define STATUS-1 "Waiting for song data") ; just started the program and waiting for song data
(define STATUS-2 "Choosing a song") ;; client is choosing a song to play
(define STATUS-3 "Requesting song") ; requested a song but have not yet received one
(define STATUS-4 SONG) ; have received a song from the server
#; (define (status-temp s)
     (cond 
       [(and (string? s) (string=? "Waiting for song data" s)) ...]
       [(and (string? s) (string=? "Choosing a song" s)) ...]
       [(and (string? s) (string=? "Requesting song" s)) ...]
       [(song? s) (song-temp s) ...]))

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
#; (define (feedback-template fs)
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
#; (define (ho1s-temp h1)
     (... (1song-history-id h1) ...
          (1song-history-name h1) ...
          (1song-history-plays h1) ...))

;; A History is a [List-of HO1S]
;; represents the all song's history
(define HISTORY-0 '())
(define HISTORY (list HO1S-1 HO1S-2))
#; (define (history-tmep h)
     (cond
       [(empty? h) ...]
       [(cons? h) ... (ho1s-temp (first h)) ...
                  (history-tmep (rest h) ...)]))

#|------------- MusicPlayer -------------|#
(define-struct musicplayer [status playlist feedback history song-chosen])
;; A musicplayer is a
;; (make-musicplayer Status [List-of IDMetaPair] FeedbackString History Number)
;; where status represents the status of the program
;; playlist is a list of IDMetaPair received from server
;; feedback is the feedback string to display to the user
;; history represents the hisotory of played music
;; song-chosen is id of a song which currently choose
(define MP-1 (make-musicplayer STATUS-1 '() "" '() 0))
(define MP-2 (make-musicplayer STATUS-2 '() BAD-FB '() 1))
(define MP-3 (make-musicplayer STATUS-3 (list IDPAIR-1) GOOD-FB HISTORY 1))
(define MP-4 (make-musicplayer STATUS-4 (list IDPAIR-1 IDPAIR-2) NO-FB HISTORY 0))
#; (define (musicplayer-temp mp)
     (... (status-temp (musicplayer-status mp))
          (loidp-temp (musicplayer-playlist mp)) ...
          (feedback-template (musicplayer-feedback mp)) ...
          (history-tmep (musicplayer-history mp)) ...
          (musicplayer-song-chosen mp) ...))

#|------------- CONSTANT -------------|#
(define FILEPATH "history.csv")
(define BG (empty-scene 1000 500))
(define NO-FB-TEXT "No feedback provided on the last song.")
(define INSTRUCTIONS "Use up and down arrow to choose songs and Click space to start playing a song.")
(define TEXT-SIZE 20)
(define ARROW (rotate 150 (triangle 15 "solid" "blue")))

#|------------- BIG BANG -------------|#
;; main/player : Any -> MusicPlayer
;; creates a musicplayer that keeps track of song history and users can choose a song to play
(define (main/player _)
  (save-history FILEPATH
                (big-bang (read-history FILEPATH)
                  [to-draw draw-playlist-history-feedback]
                  [on-key choose-song]
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
(check-expect (read-history "a new history") MP-1)
(define (read-history fpath)
  (if (file-exists? fpath)
      (make-musicplayer "Waiting for song data" '() "" (lol->history (read-csv-file fpath)) 0)
      (make-musicplayer "Waiting for song data" '() "" '() 0)))

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
;; draw-playlist-history-feedback : MusicPlayer -> Image
;; Draw the current state of the world
(check-expect (draw-playlist-history-feedback MP-1)
              (overlay (text "Waiting for song data" TEXT-SIZE "black") BG))
(check-expect (draw-playlist-history-feedback MP-2)
              (overlay (above (text INSTRUCTIONS TEXT-SIZE "red")
                              (text BAD-FB TEXT-SIZE "red"))
                       BG))
(check-expect (draw-playlist-history-feedback MP-3)
              (overlay (text "Requesting song" TEXT-SIZE "black") BG))
(check-expect (draw-playlist-history-feedback MP-4)
              (overlay (text "Press space to play." TEXT-SIZE "black") BG))
(define (draw-playlist-history-feedback mp)
  (cond 
    [(and (string? (musicplayer-status mp))
          (string=? "Waiting for song data" (musicplayer-status mp)))
     (overlay (text "Waiting for song data" TEXT-SIZE "black") BG)]
    [(and (string? (musicplayer-status mp))
          (string=? "Choosing a song" (musicplayer-status mp)))
     (overlay (above (text INSTRUCTIONS TEXT-SIZE "red")
                     (draw-feedback (musicplayer-feedback mp))
                     (draw-playlist mp))
              BG)]
    [(and (string? (musicplayer-status mp))
          (string=? "Requesting song" (musicplayer-status mp)))
     (overlay (text "Requesting song" TEXT-SIZE "black") BG)]
    [(song? (musicplayer-status mp))
     (overlay (text "Press space to play." TEXT-SIZE "black")
              BG)]))

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

;; draw-playlist : MusicPlayer -> Image
;; draw all song data once receive metadata message
(check-expect (draw-playlist MP-3)
              (text "123456     Light     0" TEXT-SIZE "black"))
(check-expect (draw-playlist
               (make-musicplayer (make-song 123456 "Light" "Rocket Girls" #"bytes")
                                 (list (list 123456 (list "Light" "Rocket Gilrs" 120 "No album")))
                                 "none" '() 123456))
              (beside ARROW (text "123456     Light     0" TEXT-SIZE "black")))
(define (draw-playlist mp)
  (local [;; draw-one-song : IDMetaPair Image -> Image
          ;; draw information of one song and combined all songs together
          (define (draw-one-song one-idpair sofar)
            (above (if (is-chosen? (first one-idpair) mp)
                       (beside ARROW
                               (text (number->string (first one-idpair)) TEXT-SIZE "black")
                               (text "     " TEXT-SIZE "black")
                               (text (first (second one-idpair)) TEXT-SIZE "black")
                               (text "     " TEXT-SIZE "black")
                               (draw-playcount one-idpair (musicplayer-history mp)))
                       (beside (text (number->string (first one-idpair)) TEXT-SIZE "black" )
                               (text "     " TEXT-SIZE "black")
                               (text (first (second one-idpair)) TEXT-SIZE  "black")
                               (text "     " TEXT-SIZE "black")
                               (draw-playcount one-idpair (musicplayer-history mp))))
                   sofar))]
    (foldr draw-one-song empty-image (musicplayer-playlist mp))))

;; is-chosen? : Nat MusicPlayer -> Boolean
;; determine if a song is chosen song 
(check-expect (is-chosen? 2 MP-2) #f)
(check-expect (is-chosen? 123456 (make-musicplayer STATUS-2 '() BAD-FB '() 123456)) #t)
(define (is-chosen? num mp)
  (= (musicplayer-song-chosen mp) num))

;; draw-playcount : IDMetaPair History -> Image
;; Draw the history of song has been played on canvas
(check-expect (draw-playcount IDPAIR-1 HISTORY-0) (text "0" TEXT-SIZE "black"))
(check-expect (draw-playcount IDPAIR-2 HISTORY) (text "5" TEXT-SIZE "black"))
(check-expect (draw-playcount IDPAIR-1 (list HO1S-2)) (text "0" TEXT-SIZE "black"))
(define (draw-playcount one-idpair h)
  (cond
    [(empty? h) (text "0" TEXT-SIZE "black")]
    [(cons? h)
     (if (record-in-file? one-idpair h)
         (if (= (1song-history-id (first h)) (first one-idpair))
             (text (number->string (1song-history-plays (first h))) TEXT-SIZE "black")
             (draw-playcount one-idpair (rest h)))
         (text "0" TEXT-SIZE "black"))]))

;; record-in-file? : IDMetaPair History -> Boolean
;; determine if there is the same id in history as one in IDMetaPair
(check-expect (record-in-file? IDPAIR-1 HISTORY-0) #f)
(check-expect (record-in-file? IDPAIR-1 HISTORY) #t)
(define (record-in-file? one-idpair h)
  (local [;; any-same-id? : HO1S -> Boolean
          ;; determine if the id of a History of one song is same as the id of a IDMetaPair
          ;; Given HO1S-1, (one-idpair=IDPAIR-1), returns #t
          ;; Given HO1S-1, (one-idpair=IDPAIR-2), returns #f
          (define (any-same-id? ho1s)
            (= (1song-history-id ho1s) (first one-idpair)))]
    (ormap any-same-id? h)))

#|--------- ON-KEY ---------|#
;; choose-song : MusicPlayer KeyEvent -> MusicPlayer
;; the user can use the arrow keys (up/down) to tab through the songs available and then use
;; enter (KeyEvent "\r") to send a request to the server to get the SongMsg for that song.
;; Once the song’s information has been downloaded, the user can press " " to play the song as before.
(check-expect (choose-song MP-1 "x") MP-1)
(check-expect (choose-song MP-1 "up") (make-musicplayer "Waiting for song data" '() "" '() 0))
(check-expect (choose-song MP-4 "down")
              (make-musicplayer
               (make-song 123456 "Light" "Rocket Girls" #"bytes")
               (list
                (list 123456 (list "Light" "Rocket Gilrs" 120 "No album"))
                (list 654321 (list "REALX" "Jack Yong" 123 "album")))
               "none"
               (list (make-1song-history 123456 "Light" 0) (make-1song-history 654321 "abc" 5)) 1))
(check-expect (choose-song MP-1 "\r")
              (make-package (make-musicplayer "Requesting song" '() "" '() 0) 0))
(define (choose-song mp ke)
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
                                (if (= (1song-history-id ho1s) (song-id (musicplayer-status mp)))
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
                      (cons (make-1song-history (song-id (musicplayer-status mp))
                                                (song-title (musicplayer-status mp))
                                                1) h))]
              (if (compare-to-history? h mp)
                  (add1-to-this-list h)
                  (build-new-list h))))]
    (cond
      [(key=? ke "up") (choose-last mp)]
      [(key=? ke "down") (choose-next mp)]
      [(key=? ke "\r") (request-bytes mp)]
      [(and (song? (musicplayer-status mp)) (key=? ke " "))
       (make-musicplayer "Choosing a song" (musicplayer-playlist mp)
                         (return-fb mp)
                         (change-history (musicplayer-history mp))
                         (musicplayer-song-chosen mp))]
      [else mp])))

;; compare-to-history? : History MusicPlayer -> Boolean
;; determine if the id of a song in history is the same as the id of song playing
(check-expect (compare-to-history? HISTORY MP-2) #f)
(check-expect (compare-to-history? HISTORY MP-4) #t)
(check-expect (compare-to-history? HISTORY-0 MP-2) #f)
(define (compare-to-history? h mp)
  (local [(define (compare-each-ho1s ho1s)
            (if (song? (musicplayer-status mp))
                (= (1song-history-id ho1s) (song-id (musicplayer-status mp)))
                #f))]
    (ormap compare-each-ho1s h)))

;; choose-last : MusicPlayer -> MusicPlayer
;; choose the last song
(check-expect (choose-last MP-1) MP-1)
(check-expect (choose-last MP-3) (make-musicplayer STATUS-3 (list IDPAIR-1) GOOD-FB HISTORY 0))
(define (choose-last mp)
  (if (> (length (musicplayer-playlist mp)) 0)
      (make-musicplayer (musicplayer-status mp)
                        (musicplayer-playlist mp)
                        (musicplayer-feedback mp) 
                        (musicplayer-history mp)
                        (modulo (- (musicplayer-song-chosen mp) 1)
                                (length (musicplayer-playlist mp))))
      mp))

;; choose-next : MusicPlayer -> MusicPlayer
;; choose the next song
(check-expect (choose-next MP-1) MP-1)
(check-expect (choose-last MP-3) (make-musicplayer STATUS-3 (list IDPAIR-1) GOOD-FB HISTORY 0))
(define (choose-next mp)
  (if (> (length (musicplayer-playlist mp)) 0)
      (make-musicplayer (musicplayer-status mp)
                        (musicplayer-playlist mp)
                        (musicplayer-feedback mp) 
                        (musicplayer-history mp)
                        (modulo (+ (musicplayer-song-chosen mp) 1)
                                (length (musicplayer-playlist mp))))
      mp))

;; request-bytes : MusicPlayer -> Package
;; request byte string to server based on the id that users choose
(check-expect (request-bytes MP-1) (make-package (make-musicplayer "Requesting song" '() "" '() 0) 0))
(define (request-bytes mp)
  (make-package (make-musicplayer "Requesting song"
                                  (musicplayer-playlist mp)
                                  (musicplayer-feedback mp)
                                  (musicplayer-history mp)
                                  (musicplayer-song-chosen mp))
                (musicplayer-song-chosen mp)))

;; return-fb : MusicPlayer -> FeedbackString
;; returns a feedback of the given player
(define (return-fb mp)
  (play-sound (song-byte-string (musicplayer-status mp))))

#|--------- ON-RECEIVE ---------|#
;; receive-message : MusicPlayer ServerMsg -> PlayerResult
;; deal with the package from server to receive a new song
;; and play it or a playlist that user can choose
(check-expect (receive-message MP-1 SMSG-1) MP-1)
(check-expect (receive-message MP-2 SMSG-2)
              (make-musicplayer (make-song 123456 "Light" "Rocket Gilrs" #"bytes")
                                '() "dislike" '() 1))
(check-expect (receive-message MP-3 SMSG-3)
              (make-musicplayer "Choosing a song" (second SMSG-3) "like"
                                (list
                                 (make-1song-history 123456 "Light" 0)
                                 (make-1song-history 654321 "abc" 5)) 1))
(define (receive-message aplayer smg)
  (cond
    [(string=? "ERROR" (first smg)) aplayer]
    [(string=? "METADATA" (first smg)) (make-musicplayer
                                        "Choosing a song"
                                        (second smg)
                                        (musicplayer-feedback aplayer)
                                        (musicplayer-history aplayer)
                                        (musicplayer-song-chosen aplayer))]
    [(string=? "SONG" (first smg)) (make-musicplayer 
                                    (make-song (second smg)
                                               (first (third smg))
                                               (second (third smg))
                                               (fourth smg))
                                    (musicplayer-playlist aplayer)
                                    (musicplayer-feedback aplayer)
                                    (musicplayer-history aplayer)
                                    (musicplayer-song-chosen aplayer))]))