;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Assignment 15|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A Section is one of:
; - Ending
; - Chapter
; and represents a section in a choose your own adventure book
 
; An Ending is a (make-ending String Boolean)
(define-struct ending [text good?])
; and represents an ending to the story with some text and whether it was a happy ever after
 
; A Chapter is a (make-chapter String [NEList-of Choice])
(define-struct chapter [text choices])
; and represents a chapter in a choose-your-own adventure book with a body
; and a list of choices at the end
 
; An [NEList-of X] (Non-Empty List) is one of:
; - (cons X '())
; - (cons X [NEList-of X])
 
; A Choice is a a (make-choice String Section)
(define-struct choice [text result])
; and represents the blurb shown to the reader and the resulting section if that is the
; choice they make
(define MY-STORY
  (make-chapter
   "You are alone in a room. There is a door before you."
   (list
    (make-choice
     "Stay in the room."
     (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
    (make-choice
     "Open the door."
     (make-chapter
      "You open the door. On the other side lays an eternity of nothingness."
      (list
       (make-choice
        "Scream."
        (make-ending
         "You scream into the void. There is no response. There never will be." #f))
       (make-choice
        "Accept your fate."
        (make-ending
         "You accept the simple beauty of the void. You achieve nirvana." #t))))))))

;; Exercise 1
#;
(define (choice-temp c)
  (... (choice-text c)
       (section-temp (choice-result c)) ...))
#;
(define (list-of-choice-temp loc)
  (cond
    [(empty? loc) ...]
    [(cons? loc)
     ( ... (choice-temp (first loc)) ...
           (list-of-choice-temp (rest loc)) ...)]))
#;
(define (chapter-temp cp)
  (... (chapter-text cp)
       (list-of-choice-temp (chapter-choices cp)) ...))
#;
(define (ending-tep e)
  (... (ending-text s) ...
       (ending-good? s) ...))
#;
(define (section-temp s)
  (cond
    [(ending? s) ... (ending-temp s) ...]
    [(chapter? s) ... (chapter-temp s) ...]))

;; Exercise 2
;; total-ending : Section -> Number
;; given a Section, produces the total number of possible endings to the story
(check-expect (total-ending MY-STORY) 3)
(check-expect (total-ending (make-ending "You achieve nirvana." #true)) 1)
(define (total-ending s)
  (cond
    [(ending? s) 1]
    [(chapter? s) (ending-in-chapter s)]))

;; ending-in-chapter : Chapter -> Number
;; given a Chapter, produces the total number of possible endings to the story
(check-expect (ending-in-chapter MY-STORY) 3)
(check-expect
 (ending-in-chapter
  (make-chapter "aaa"
                (list (make-choice "" (make-ending "qwert" #f))
                      (make-choice "" (make-ending "qwert" #f))
                      (make-choice "" (make-chapter "a"
                                                    (list (make-choice "p" (make-ending "p" #f))
                                                          (make-choice "p" (make-ending "p" #f))))))))
 4)
(define (ending-in-chapter cp)
  (ending-in-loc (chapter-choices cp)))

;; ending-in-loc : [List-of Choice] -> Number
;; given a List of Choice, produces the total number of possible endings to the story
(check-expect (ending-in-loc (list (make-choice "Stay in the room."
                                                (make-ending "Nothing happens." #false)))) 1)
(define (ending-in-loc loc)
  (foldr ending-in-choice 0 loc))

;; ending-in-choice : Choice Number -> Number
;; given a Choice, produces the total number of possible endings to the story plus a number
(check-expect (ending-in-choice (make-choice "Stay in the room."
                                             (make-ending "Nothing happens." #false)) 5) 6)
(define (ending-in-choice c n)
  (+ n (total-ending (choice-result c))))

;; Exercise 3
;; constants:
(define TEXT-SIZE 20)
(define TEXT-COLOR "black")
(define BG (empty-scene 800 400))
(define ARROW (rotate 150 (triangle 15 "solid" "blue")))

(define-struct game [section current-choice])
;; where section is a section of a story (Ending/Chapter)
;; current-choice is a choice goona make
(define GAME
  (make-game MY-STORY
             (make-choice
              "Stay in the room."
              (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))))
(define ENDING-GAME
  (make-game (make-ending "Nothing ever will again." #f)
             (make-choice "" (make-ending "" #t))))
#;
(define (game-temp g)
  ( ... (section-temp (game-section g)) ...
        (choice-temp (game-current-choice g)) ...))

;; main/game : Section -> Game
;; takes in a Section and allows the player to move through the story
(define (main/game s)
  (output-text (big-bang (game-produce s)
                 [to-draw draw-game]
                 [on-key move-arrow]
                 [stop-when is-it-end? draw-ending])))

;; game-produce : Section -> Game
;; given a Section, produces a game which contains the original Section
;; and the first choice of choices of chapter or (make-choice "" (make-ending "" #t))
(check-expect
 (game-produce MY-STORY)
 (make-game MY-STORY
            (make-choice
             "Stay in the room."
             (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #false))))
(check-expect (game-produce (make-ending "cool" #t))
              (make-game (make-ending "cool" #t)
                         (make-choice "" (make-ending "" #t))))
(define (game-produce s)
  (cond
    [(ending? s) (make-game s (make-choice "" (make-ending "" #t)))]
    [(chapter? s) (make-game s (first (chapter-choices s)))]))

;; output-text : Game -> String
;; given a Section, returns the text in it
(check-expect (output-text ENDING-GAME) "Nothing ever will again.")
(check-expect (output-text GAME) "You are alone in a room. There is a door before you.")
(define (output-text g)
  (cond
    [(ending? (game-section g)) (ending-text (game-section g))]
    [(chapter? (game-section g)) (chapter-text (game-section g))]))

#|---------------- To-Draw ----------------|#
;; draw-game : Game -> Image
;; draw the text and choices of a section in a game
(check-expect (draw-game GAME)
              (overlay
               (above
                (text "You are alone in a room. There is a door before you." TEXT-SIZE TEXT-COLOR)
                (beside ARROW (text "Stay in the room." TEXT-SIZE TEXT-COLOR))
                (text "Open the door." TEXT-SIZE TEXT-COLOR))
               BG))
(define (draw-game g)
  (local [;; draw-chapter : Chapter -> Image
          ;; shows the text of the section, and beneath it, a list of the choices they have
          (define (draw-chapter cp)
            (local [;; draw-loc : [List-of Choice] -> Image
                    ;; show all choices that players can choose
                    (define (draw-loc loc)
                      (cond
                        [(empty? loc) empty-image]
                        [(cons? loc)
                         (if (string=? (choice-text (first loc))
                                       (choice-text (game-current-choice g)))
                             (above
                              (beside ARROW (text (choice-text (first loc)) TEXT-SIZE TEXT-COLOR))
                              (draw-loc (rest loc)))
                             (above (text (choice-text (first loc)) TEXT-SIZE TEXT-COLOR)
                                    (draw-loc (rest loc))))]))]
              (overlay (above (text (chapter-text cp) TEXT-SIZE TEXT-COLOR)
                              (draw-loc (chapter-choices cp)))
                       BG)))]
    (draw-chapter (game-section g))))

#|---------------- On-Key ----------------|#
;; move-arrow : Game KeyEvent -> Game
;; use the arrow keys (up/down) to tab through the choices available and
;; then use enter (KeyEvent "\r") to move on to the selected result
(check-expect (move-arrow GAME "x") GAME)
(check-expect
 (move-arrow GAME "up")
 (make-game
  MY-STORY
  (make-choice
   "Open the door."
   (make-chapter
    "You open the door. On the other side lays an eternity of nothingness."
    (list
     (make-choice
      "Scream."
      (make-ending
       "You scream into the void. There is no response. There never will be." #false))
     (make-choice
      "Accept your fate."
      (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true)))))))
(check-expect
 (move-arrow GAME "down")
 (make-game
  MY-STORY
  (make-choice
   "Open the door."
   (make-chapter
    "You open the door. On the other side lays an eternity of nothingness."
    (list
     (make-choice
      "Scream."
      (make-ending
       "You scream into the void. There is no response. There never will be." #false))
     (make-choice
      "Accept your fate."
      (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true)))))))
(check-expect
 (move-arrow GAME "\r")
 (make-game (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #false)
            (make-choice "" (make-ending "" #t))))
(define (move-arrow g ke)
  (cond
    [(key=? ke "up") (make-game (game-section g) (last-choice g))]
                                   
    [(key=? ke "down") (make-game (game-section g) (next-choice g))]
    [(key=? ke "\r") (result g)]
    [else g]))

;; last-choice : Game -> Choice
;; Given a Game, and returns the last choice 
(check-expect
 (last-choice GAME)
 (make-choice
  "Open the door."
  (make-chapter
   "You open the door. On the other side lays an eternity of nothingness."
   (list
    (make-choice
     "Scream."
     (make-ending
      "You scream into the void. There is no response. There never will be." #false))
    (make-choice
     "Accept your fate."
     (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true))))))
(define (last-choice g)
  (local [;; choice-index : [List-of Choices] -> Number
          ;; check the index of a choice of Game in list of choices in Section
          (define (choice-index loc)
            (cond
              [(empty? loc) 0]
              [(cons? loc)
               (if (string=? (choice-text (first loc)) (choice-text (game-current-choice g)))
                   0
                   (add1 (choice-index (rest loc))))]))]
    (if (= (choice-index (chapter-choices (game-section g))) 0)
        (make-choice (choice-text
                      (list-ref (chapter-choices (game-section g))
                                (- (length (chapter-choices (game-section g))) 1)))
                     (choice-result
                      (list-ref (chapter-choices (game-section g))
                                (- (length (chapter-choices (game-section g))) 1))))
        (make-choice (choice-text
                      (list-ref (chapter-choices (game-section g))
                                (- (choice-index (chapter-choices (game-section g))) 1)))
                     (choice-result
                      (list-ref (chapter-choices (game-section g))
                                (- (choice-index (chapter-choices (game-section g))) 1)))))))

;; next-choice : Game -> Choice
;; Given a Game, and returns the last choice 
(check-expect
 (next-choice GAME)
 (make-choice
  "Open the door."
  (make-chapter
   "You open the door. On the other side lays an eternity of nothingness."
   (list
    (make-choice
     "Scream."
     (make-ending
      "You scream into the void. There is no response. There never will be." #false))
    (make-choice
     "Accept your fate."
     (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true))))))
(define (next-choice g)
  (local [;; choice-index : [List-of Choices] -> Number
          ;; check the index of a choice of Game in list of choices in Section
          (define (choice-index loc)
            (cond
              [(empty? loc) 0]
              [(cons? loc)
               (if (string=? (choice-text (first loc)) (choice-text (game-current-choice g)))
                   0
                   (add1 (choice-index (rest loc))))]))]
    (if (= (choice-index (chapter-choices (game-section g)))
           (- (length (chapter-choices (game-section g))) 1))
        (make-choice (choice-text
                      (list-ref (chapter-choices (game-section g))
                                0))
                     (choice-result
                      (list-ref (chapter-choices (game-section g))
                                0)))
        (make-choice (choice-text
                      (list-ref (chapter-choices (game-section g))
                                (+ (choice-index (chapter-choices (game-section g))) 1)))
                     (choice-result
                      (list-ref (chapter-choices (game-section g))
                                (+ (choice-index (chapter-choices (game-section g))) 1)))))))

;; result : GAME -> GAME
;; Given a Game, and returns a new Game which contains a new Section after making this choice
;; if new Section is Chapter, choice is the first element in choices
;; if new Section is Ending, choice is (make-choice "" (make-ending "" #t))
(check-expect (result GAME)
              (make-game
               (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f)
               (make-choice "" (make-ending "" #t))))
(define (result g)
  (local [;; choice-index : [List-of Choices] -> Number
          ;; check the index of a choice of Game in list of choices in Section
          (define (choice-index loc)
            (cond
              [(empty? loc) 0]
              [(cons? loc)
               (if (string=? (choice-text (first loc)) (choice-text (game-current-choice g)))
                   0
                   (add1 (choice-index (rest loc))))]))]
    (if (chapter? (choice-result (list-ref (chapter-choices (game-section g))
                                           (choice-index (chapter-choices (game-section g))))))
        (make-game (choice-result (list-ref (chapter-choices (game-section g))
                                            (choice-index (chapter-choices (game-section g)))))
                   (list-ref
                    (chapter-choices (choice-result
                                      (list-ref (chapter-choices (game-section g))
                                                (choice-index (chapter-choices (game-section g))))))
                    0))
        (make-game (choice-result (list-ref (chapter-choices (game-section g))
                                            (choice-index (chapter-choices (game-section g)))))
                   (make-choice "" (make-ending "" #t))))))
                              
#|---------------- Stop-When ----------------|#
;; is-it-end? : Game -> Boolean
;; determines if the game is end
(check-expect (is-it-end? GAME) #f)
(check-expect (is-it-end? (make-game
                           (make-ending "Nothing happens." #f)
                           (make-choice "empty" (make-ending "xxx" #t)))) #t)
(define (is-it-end? g)
  (ending? (game-section g)))

;; draw-ending : Game -> Image
;; display the text of the ending in blue if it is a happy ever after and in red if it is not
(check-expect (draw-ending ENDING-GAME)
              (overlay (text "Nothing ever will again." TEXT-SIZE "red")
                       BG))
(check-expect (draw-ending (make-game (make-ending "cool" #t)
                                      (make-choice "" (make-ending "" #t))))
              (overlay (text "cool" TEXT-SIZE "blue")
                       BG))
(define (draw-ending g)
  (cond
    [(boolean=? #t (ending-good? (game-section g)))
     (overlay (text (ending-text (game-section g)) TEXT-SIZE "blue")
              BG)]
    [(boolean=? #f (ending-good? (game-section g)))
     (overlay (text (ending-text (game-section g)) TEXT-SIZE "red")
              BG)]))

;; Exercise 4
; A PlayerSimulation is a [List-of KO]
; and represents the key events a player makes when playing a choose your own adventure game
(define PS (list "up" "up" "enter" "enter"))
(define PS-HAPPY (list "down" "enter" "down" "enter"))
; A KO (Key Option) is one of:
; - "up"
; - "down"
; - "enter"
; and represents either pressing the up arrow, down arrow, or enter/return key

; A Result is one of:
; - "happy"
; - "sad"
; - "incomplete"
; and represents whether a story ended happilly, sadly, or did not yet end

;; final-result : Section PlayerSimulation -> Result
;; given a Section and a PlayerSimulation, produces a Result representing
;; the state of the story after following PlayerSimulation
(check-expect (final-result MY-STORY PS) "sad")
(check-expect (final-result MY-STORY PS-HAPPY) "happy")
(check-expect (final-result MY-STORY '()) "incomplete")
(define (final-result s ps)
  (if (ending? (game-section (robot-choice (make-game s (first (chapter-choices s))) ps)))
      (test (game-section (robot-choice (make-game s (first (chapter-choices s))) ps)))
      (test s)))

;; robot-choice : Game PlayerSimulation -> Game
;; simulates Game based on given PlayerSimulation, and returns the final Game
(check-expect (robot-choice GAME
                            PS-HAPPY)
              (make-game
               (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true)
               (make-choice "" (make-ending "" #t))))
(check-expect (robot-choice GAME '()) GAME)
(check-expect (robot-choice (make-game (make-ending "" #t) (make-choice "" (make-ending "" #f))) '())
              (make-game (make-ending "" #t) (make-choice "" (make-ending "" #f))))
(define (robot-choice g ps)
  (cond 
    [(empty? ps) g]
    [(cons? ps)
     (robot-choice (move-choice g (first ps))
                   (rest ps))]))

;; move-choice : Game KeyEvent -> Game
;; given PlayerSimulation which represents list of KeyEvent,use the arrow keys (up/down)
;; to tab through the choices available and then use enter (KeyEvent "\r") to move on to
;; the selected result
(check-expect (move-choice GAME "x") GAME)
(check-expect
 (move-choice GAME "up")
 (make-game
  MY-STORY
  (make-choice
   "Open the door."
   (make-chapter
    "You open the door. On the other side lays an eternity of nothingness."
    (list
     (make-choice
      "Scream."
      (make-ending "You scream into the void. There is no response. There never will be." #false))
     (make-choice
      "Accept your fate."
      (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true)))))))
(check-expect
 (move-choice GAME "enter")
 (make-game (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #false)
            (make-choice "" (make-ending "" #t))))
(define (move-choice g ke)
  (cond [(ending? (game-section g)) g]
        [(string=? ke "up") (make-game (game-section g) (last-choice g))]        
        [(string=? ke "down") (make-game (game-section g) (next-choice g))]
        [(string=? ke "enter") (result g)]
        [else g]))

;; test : Section -> Result
;; given a Section, check its ending good or not
;; returns "sad" if it is a bad ending 
;; returns "happy" if it is a good ending
;; returns "incomplete" if it does not finished
(check-expect (test (make-ending "abc" #f)) "sad")
(check-expect (test (make-ending "hoooo" #t)) "happy")
(check-expect (test MY-STORY) "incomplete")
(define (test s)
  (cond
    [(and (ending? s) (boolean=? #f (ending-good? s))) "sad"]
    [(and (ending? s) (boolean=? #t (ending-good? s))) "happy"]
    [(chapter? s) "incomplete"]))

;; Exercise 5
;; SimulationResult is one of:
;; - PlayerSimulation
;; - #f
(define SR-1 PS-HAPPY)
(define SR-2 #f)
    
;; find-happily-ever-after : Section -> SimulationResult
;; given a Section, produces a PlayerSimulation which results in a happy ever after
;; If there is no happy ever after to be found, the function should produce #f
(check-expect (find-happily-ever-after MY-STORY) PS-HAPPY)
(check-expect (find-happily-ever-after
               (make-chapter "adf"
                             (list (make-choice "adsf" (make-ending "qwer" #f))
                                   (make-choice "adsf" (make-ending "qwer" #f))))) #f)
(define (find-happily-ever-after s)
  (cond [(and (ending? s) (boolean=? #f (ending-good? s))) #f]
        [(and (ending? s) (boolean=? #t (ending-good? s))) '()]
        [(ormap any-good? (all-booleans s)) (path-to-happy s)]
        [else  #f]))

;; all-booleans : Section -> [List-of Boolean]
;; returns all booleans which represents the ending is good or not in a Section
(check-expect (all-booleans MY-STORY) (list #f #f #t))
(define (all-booleans s)
  (cond
    [(ending? s) (cons (ending-good? s) '())]
    [(chapter? s) (booleans-in-chapter s)]))

;; booleans-in-chapter : Section -> [List-of Boolean]
;; returns all booleans which represents the ending is good or not in a Chapter
(check-expect (booleans-in-chapter MY-STORY) (list #f #f #t))
(define (booleans-in-chapter cp)
  (booleans-in-loc (chapter-choices cp)))

;; booleans-in-loc : [List-of Choice] -> [List-of Boolean]
;; returns all booleans which represents the ending is good or not in a List of Choice
(check-expect
 (booleans-in-loc
  (list
   (make-choice
    "Stay in the room."
    (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #false))
   (make-choice
    "Open the door."
    (make-chapter
     "You open the door. On the other side lays an eternity of nothingness."
     (list
      (make-choice "Scream."
                   (make-ending
                    "You scream into the void. There is no response. There never will be." #false))
      (make-choice
       "Accept your fate."
       (make-ending "You accept the simple beauty of the void. You achieve nirvana." #true)))))))
 (list #f #f #t))
(define (booleans-in-loc loc)
  (foldr boolean-in-choice '() loc))

;; boolean-in-choice : Choice [List-of Boolean] -> [List-of Boolean]
;; returns all booleans which represents the ending is good or not in a single Choice
;; and append all Booleans together
(check-expect (boolean-in-choice (make-choice "" (make-ending "qwert" #f))
                                 (list #f #t)) (list #f #f #t))
(define (boolean-in-choice c lob)
  (append (all-booleans (choice-result c)) lob))

;; any-good? : Boolean -> Boolean
;; determine if Boolean is #t
(check-expect (any-good? #t) #t)
(check-expect (any-good? #f) #f)
(define (any-good? b)
  (boolean=? #t b))

;; path-to-happy : Section -> SimulationResult
;; given a Section, produces a PlayerSimulation which results in a happy ever after
(check-expect (path-to-happy MY-STORY) PS-HAPPY)
(define (path-to-happy s)
  (local [(define (enter-or-down c ps)
            (cond
              [(chapter? (choice-result c))
               (cons "enter" (path-to-happy (choice-result c)))]
              [(and (ending? (choice-result c)) (boolean=? #t (ending-good? (choice-result c))))
               (cons "enter" ps)]
              [(and (ending? (choice-result c)) (boolean=? #f (ending-good? (choice-result c))))
               (cons "down" ps)]))]
    (foldr enter-or-down '() (chapter-choices s))))