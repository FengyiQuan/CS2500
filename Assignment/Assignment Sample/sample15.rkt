;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname sample15) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
(require 2htdp/image)
(require 2htdp/universe)

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
; and represents the blurb shown to the reader and the resulting section if that is the choice they
; make

;; An MPS is a [Maybe PlayerSimulation]
 
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

;; section-template : Section -> ?
(define (section-template section)
  (cond [(ending? section) (ending-temp section)]
        [(chapter? section) (chapter-temp section)]))

;; ending-temp : Ending -> ?
(define (ending-temp ending)
  (... (ending-text ending) (ending-good? ending)))

;; chapter-temp : Chapter -> ?
(define (chapter-temp chapter)
  (... (chapter-text chapter) (choices-temp (chapter-choices chapter))))

;; choices-temp : [NEList-of Choice] -> ?
(define (choices-temp loc)
  (cond [(empty? (rest loc))
         (... (choice-temp (first loc)) ...)]
        [(cons? (rest loc))
         (... (choice-temp (first loc))
              (choices-temp (rest loc)))]))

;; choice-temp : Choice -> ?
(define (choice-temp choice)
  (... (choice-text choice) (section-template (choice-result choice))))



;; Exercise 2

(define ENDING1
  (make-ending "You stay in the room. Nothing happens. Nothing ever will again." #f))
(define ENDING2
  (make-ending "You scream into the void. There is no response. There never will be." #f))
(define ENDING3
  (make-ending "You accept the simple beauty of the void. You achieve nirvana." #t))

(define CHOICE1 (make-choice "Stay in the room." ENDING1))
(define CHOICE3 (make-choice "Scream." ENDING2))
(define CHOICE4 (make-choice "Accept your fate." ENDING3))

(define CHAPTER2
  (make-chapter "You open the door. On the other side lays an eternity of nothingness."
                (list CHOICE3 CHOICE4)))
(define CHOICE2 (make-choice "Open the door." CHAPTER2))

(define CHAPTER1 MY-STORY)

;; total-endings : Section -> Number
;; The number of endings that can take place
(check-expect (total-endings MY-STORY) 3)
(check-expect (total-endings CHAPTER2) 2)
(define (total-endings section)
  (local [;; total-endings/chapter : Chapter -> Number
          ;; Total endings for a chapter
          (define (total-endings/chapter chapter)
            (total-endings/choices (chapter-choices chapter)))
          ;; total-endings/choices : [List-of Choice] -> Number
          ;; The total endings reachable from these choices
          (define (total-endings/choices choices)
            (foldr add-endings 0 choices))
          ;; add-endings : Choice Number -> Number
          ;; Add the # of paths in this choice to the answer
          (define (add-endings choice n)
            (+ (endings/choice choice) n))
          ;; endings/choice : Choice -> Number
          ;; Total number of endings from this choice
          (define (endings/choice choice)
            (total-endings (choice-result choice)))]
    (cond [(ending? section) 1]
          [(chapter? section) (total-endings/chapter section)])))


;; Exercise 3

;; An Adventure is a (make-adventure Section Nat)
(define-struct adventure [section index])
;; and represents the current section and current highlighted choice

(define ADVENTURE-0 (make-adventure MY-STORY 0))

;; adventure-temp : Adventure -> ?
(define (adventure-temp adventure)
  (... (section-temp (adventure-temp section)) (adventure-index adventure)))

;; main/story : Section -> String
;; Run choose your own adventure
(define (main/story section)
  (section-text
   (adventure-section
    (big-bang (make-adventure section 0)
      [to-draw draw-adventure]
      [on-key handle-key]
      [stop-when (compose ending? adventure-section) draw-adventure]))))

;; section-text : Section -> String
;; Return the text of the section
(check-expect
 (section-text CHAPTER2)
 "You open the door. On the other side lays an eternity of nothingness.")
(check-expect
 (section-text ENDING1)
 "You stay in the room. Nothing happens. Nothing ever will again.")
(define (section-text section)
  (cond [(chapter? section) (chapter-text section)]
        [(ending? section) (ending-text section)]))

(define BG (empty-scene 500 500))
(define PAD (rectangle 20 20 "outline" "white"))

;; draw-adventure : Adventure -> Image
;; Draw the game
(check-expect
 (draw-adventure ADVENTURE-0)
 (overlay
  (above (draw-text "You are alone in a room. There is a door before you." "black")
         PAD
         (draw-text "Stay in the room." "gray")
         PAD
         (draw-text "Open the door." "black")
         PAD)
  BG))
(check-expect
 (draw-adventure (make-adventure (make-ending "You die." #f) 0))
 (overlay (draw-text "You die." "red") BG))
(check-expect
 (draw-adventure (make-adventure (make-ending "You win." #t) 0))
 (overlay (draw-text "You win." "blue") BG))
(define (draw-adventure adventure)
  (local [;; draw-section : Section -> Image
          ;; Draw the section
          (define (draw-section section)
            (cond [(ending? section) (draw-ending section)]
                  [(chapter? section) (draw-chapter section)]))
          ;; draw-chapter : Chapter -> Image
          ;; Draw a chapter
          (define (draw-chapter chapter)
            (overlay
             (above (draw-text (chapter-text chapter) "black")
                    PAD
                    (draw-choices (chapter-choices chapter) (adventure-index adventure)))
             BG))]
    (draw-section (adventure-section adventure))))

;; draw-ending : Ending -> Image
;; Draw an ending
(check-expect
 (draw-ending ENDING2)
 (overlay (draw-text "You scream into the void. There is no response. There never will be." "red")
          BG))
(check-expect
 (draw-ending ENDING3)
 (overlay (draw-text "You accept the simple beauty of the void. You achieve nirvana." "blue") BG))
(define (draw-ending ending)
  (overlay (draw-text (ending-text ending) (if (ending-good? ending) "blue" "red")) BG))

;; draw-text : String Color -> Image
;; Draw the text in the given color
(check-expect (draw-text "lalala" "black") (text "lalala" 15 "black"))
(check-expect (draw-text "hello there" "blue") (text "hello there" 15 "blue"))
(define (draw-text str acolor)
  (text str 15 acolor))

;; draw-choices : [List-of Choice] Integer -> Image
;; Draw choices in black, except for the one at n, which is in grey
;; n < (length loc)
(check-expect (draw-choices '() -1) empty-image)
(check-expect
 (draw-choices (list CHOICE1 CHOICE2) 0)
 (above (draw-text "Stay in the room." "gray")
        PAD
        (draw-text "Open the door." "black")
        PAD))
(check-expect
 (draw-choices (list CHOICE1 CHOICE2) 1)
 (above (draw-text "Stay in the room." "black")
        PAD
        (draw-text "Open the door." "gray")
        PAD))
(define (draw-choices loc n)
  (cond [(empty? loc) empty-image]
        [(cons? loc) (above (draw-text (choice-text (first loc)) (if (zero? n) "gray" "black"))
                            PAD
                            (draw-choices (rest loc) (sub1 n)))]))
;; Notice that we can't use a list abstraction above since we are recurring over TWO things: the
;; list AND the natural number. There is an alternative way to do this using build-list.

;; handle-key : Adventure KeyEvent -> Adventure
;; Tab through choices/make choice
(check-expect (handle-key ADVENTURE-0 "up") (move-index ADVENTURE-0 -1))
(check-expect (handle-key ADVENTURE-0 "down") (move-index ADVENTURE-0 1))
(check-expect (handle-key ADVENTURE-0 "\r") (choose ADVENTURE-0))
(check-expect (handle-key ADVENTURE-0 "a") ADVENTURE-0)
(define (handle-key adventure ke)
  (cond [(key=? "up" ke) (move-index adventure -1)]
        [(key=? "down" ke) (move-index adventure 1)]
        [(key=? "\r" ke) (choose adventure)]
        [else adventure]))

;; move-index : Adventure Nat -> Adventure
;; Move index by n
(check-expect (move-index ADVENTURE-0 2) ADVENTURE-0)
(check-expect (move-index ADVENTURE-0 1) (make-adventure MY-STORY 1))
(check-expect (move-index (make-adventure ENDING1 0) 2) (make-adventure ENDING1 0))
(define (move-index adventure n)
  (local [;; move-index/section : Section -> Nat
          ;; Move adventure's index by n without going out of the choices for the section
          (define (move-index/section section)
            (cond [(ending? section) (adventure-index adventure)]
                  [(chapter? section) (move-index/chapter section)]))
          ;; move-index/chapter : Chapter -> Nat
          ;; Move adventure's index by n without going out of the choices for the chapter
          (define (move-index/chapter chapter)
            (modulo (+ n (adventure-index adventure))
                    (length (chapter-choices chapter))))]
    (make-adventure (adventure-section adventure)
                    (move-index/section (adventure-section adventure)))))

;; choose : Adventure -> Adventure
;; Move onto the next section
(check-expect (choose ADVENTURE-0) (make-adventure ENDING1 0))
(check-expect (choose (make-adventure ENDING1 0)) (make-adventure ENDING1 0))
(define (choose adventure)
  (local [;; choose/section : Section -> Section
          ;; Select the choice in section at adventure's index
          (define (choose/section section)
            (cond [(ending? section) section]
                  [(chapter? section) (choose/chapter section)]))
          ;; choose/chapter : Chapter -> Section
          ;; Select the choice in chapter at adventure's index
          (define (choose/chapter chapter)
            (choice-result (list-ref (chapter-choices chapter) (adventure-index adventure))))]
    (make-adventure (choose/section (adventure-section adventure)) 0)))
  
;; Exercise 4

; A PlayerSimulation is a [List-of KO]
; and represents the key events a player makes when playing a choose your own adventure game
 
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

;;; NO ACCUMULATOR SOLUTION 

;; result : Section PlayerSimulation -> Result
;; Simulate a player on this section
(check-expect (result (make-ending "" #f) '()) "sad")
(check-expect (result MY-STORY '()) "incomplete")
(check-expect (result MY-STORY (list "down" "enter" "down" "enter")) "happy")
(check-expect (result MY-STORY (list "down" "enter" "down" "enter" "enter")) "happy")
(check-expect (result MY-STORY (list "up" "up" "enter")) "sad")
(check-expect (result (make-chapter "" (list (make-choice "" (make-ending "" #f))
                                             (make-choice "" (make-ending "" #f))
                                             (make-choice "" (make-ending "" #t))))
                      (list "down" "up" "down" "enter")) "sad")
(check-expect (result (make-chapter "" (list (make-choice "" (make-ending "" #f))
                                             (make-choice "" (make-ending "" #f))
                                             (make-choice "" (make-ending "" #t))))
                      (list "up" "enter")) "happy")
(define (result section ps)
  (cond [(or (ending? section) (empty? ps)) (describe-section section)]
        [(and (chapter? section) (cons? ps)) (result (apply-ko section (first ps))
                                                     (rest ps))]))

;; describe-section : Section -> Result
;; Describe the section
(check-expect (describe-section MY-STORY) "incomplete")
(check-expect (describe-section (make-ending "" #f)) "sad")
(check-expect (describe-section (make-ending "" #t)) "happy")
(define (describe-section section)
  (local [;; describe-ending : Ending -> Result
          ;; Describe the ending
          (define (describe-ending ending)
            (if (ending-good? ending) "happy" "sad"))]
    (cond [(ending? section) (describe-ending section)]
          [(chapter? section) "incomplete"])))

;; apply-ko : Chapter KO -> Section
;; Apply the key option to this section
(check-expect (apply-ko MY-STORY "up") (put-last-first MY-STORY))
(check-expect (apply-ko MY-STORY "down") (put-first-last MY-STORY))
(check-expect (apply-ko MY-STORY "enter") (pick-first MY-STORY))
(define (apply-ko chapter ko)
  (cond [(string=? ko "up") (put-last-first chapter)]
        [(string=? ko "down") (put-first-last chapter)]
        [(string=? ko "enter") (pick-first chapter)]))

;; put-last-first : Chapter -> Chapter
;; Put the last option first
(check-expect (put-last-first (make-chapter "" (list (make-choice "" (make-ending "a" #f))
                                                     (make-choice "" (make-ending "b" #f))
                                                     (make-choice "" (make-ending "c" #f)))))
              (make-chapter "" (list (make-choice "" (make-ending "c" #f))
                                     (make-choice "" (make-ending "a" #f))
                                     (make-choice "" (make-ending "b" #f)))))
(define (put-last-first chapter)
  (local [(define choices (chapter-choices chapter))
          (define last (first (reverse choices)))
          (define all-but-last (reverse (rest (reverse choices))))]
    (make-chapter (chapter-text chapter) (cons last all-but-last))))

;; put-first-last : Chapter -> Chapter
;; Put the first option last
(check-expect (put-first-last (make-chapter "" (list (make-choice "" (make-ending "a" #f))
                                                     (make-choice "" (make-ending "b" #f))
                                                     (make-choice "" (make-ending "c" #f)))))
              (make-chapter "" (list (make-choice "" (make-ending "b" #f))
                                     (make-choice "" (make-ending "c" #f))
                                     (make-choice "" (make-ending "a" #f)))))
(define (put-first-last chapter)
  (local [(define choices (chapter-choices chapter))]
    (make-chapter (chapter-text chapter) (append (rest choices) (list (first choices))))))

;; pick-first : Chapter -> Section
;; Pick the first section in the chapter
(check-expect (pick-first (make-chapter "" (list (make-choice "" (make-ending "a" #f))
                                                 (make-choice "" (make-ending "b" #f))
                                                 (make-choice "" (make-ending "c" #f)))))
              (make-ending "a" #f))
(define pick-first (compose choice-result first chapter-choices))

;; Exercise 5

;; find-happily-ever-after : Section -> MPS
;; Find the left most happily ever after in section (if present)
(check-expect (find-happily-ever-after MY-STORY) (list "down" "enter" "down" "enter"))
(check-expect (find-happily-ever-after (pick-first MY-STORY)) #f)
(check-expect (find-happily-ever-after (make-ending "" #f)) #f)
(check-expect (find-happily-ever-after (make-ending "" #t)) '())
(check-expect (find-happily-ever-after
               (make-chapter "" (list (make-choice "" (make-ending "" #f))
                                      (make-choice "" (make-ending "" #f)))))
              #f)
(check-expect (find-happily-ever-after
               (make-chapter "" (list (make-choice "" (make-ending "" #f))
                                      (make-choice "" (make-ending "" #f))
                                      (make-choice "" (make-ending "" #t)))))
              (list "down" "down" "enter"))
(check-expect (find-happily-ever-after
               (make-chapter
                ""
                (list (make-choice "" (make-ending "" #f))
                      (make-choice "" (make-ending "" #f))
                      (make-choice ""
                                   (make-chapter "" (list (make-choice "" (make-ending "" #t))))))))
              (list "down" "down" "enter" "enter"))
(define (find-happily-ever-after section)
  (cond [(ending? section) (find-happily-ever-after/ending section)]
        [(chapter? section) (find-happily-ever-after/chapter section)]))

;; find-happily-ever-after/ending : Ending -> MPS
;; '() if happily ever after, else #f
(check-expect (find-happily-ever-after/ending ENDING1) #f)
(check-expect (find-happily-ever-after/ending ENDING3) '())
(define (find-happily-ever-after/ending ending)
  (if (ending-good? ending) '() #f))

;; find-happily-ever-after/chapter : Chapter -> MPS
;; Find the left most happily ever after in chapter (if present)
(check-expect (find-happily-ever-after/chapter CHAPTER1) (list "down" "enter" "down" "enter"))
(check-expect (find-happily-ever-after/chapter CHAPTER2) (list "down" "enter"))
(define (find-happily-ever-after/chapter chapter)
  (find-happily-ever-after/choices (chapter-choices chapter)))

;; find-happily-ever-after/choices : [List-of Choice] -> MPS
;; Find the left-most happily ever after in the choices (if any)
(check-expect (find-happily-ever-after/choices '()) #false)
(check-expect (find-happily-ever-after/choices (list CHOICE1 CHOICE2))
              (list "down" "enter" "down" "enter"))
(define (find-happily-ever-after/choices choices)
  (local [;; try-choice : Choice MPS -> MPS
          ;; If we have not found a happy ending, try this choice
          (define (try-choice c sofar)
            (cond [(false? sofar) (cons-if-list "enter" (find-happily-ever-after/choice c))]
                  [(list? sofar) (cons "down" sofar)]))

          ;; cons-if-list : KO MPS -> MPS
          ;; Add the given key option if this is a list
          (define (cons-if-list ko mps)
            (cond [(false? mps) mps]
                  [(list? mps) (cons ko mps)]))]
          (foldr try-choice #false choices)))

;; find-happily-ever-after/choice : Choice -> MPS
;; Find the happily ever after (if present)
(define (find-happily-ever-after/choice choice)
  (find-happily-ever-after (choice-result choice)))