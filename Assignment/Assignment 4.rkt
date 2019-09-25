;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |Assignment 4|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; Assignment 4
;; Exercise 1
(define-struct songs-play [song1 song2 feedback])
;; a song-play is (make-song string string feedback)
;; whhere song1 is a name of the song being played currently
;; snog2 is the a name of the song being played next
;; feedback is word being tracked during song1 is being played
;; feedback is one of:
;; - "like"
;; - "dislike"
;; - "none"

;; Examples:
(define SONGS-PLAY-1 (make-songs-play "abc" "def" "like"))
(define SONGS-PLAY-2 (make-songs-play "" "" "dislike"))
(define SONGS-PLAY-3 (make-songs-play "apologize" "Hello" "none"))

#;
(define (songs-play-temp sp)
  (... (songs-play-song1 sp) ...
       ... (songs-play-song2 sp) ...
       ... (songs-play-feedback sp) ...))

;; Exercise 2
;; manhattan-distance : Posn Posn -> Number
;; computes the distance between two points along the grid lines

#;
(define (manhattan-distance-temp pos manhattan)
  (... (posn-x pos) ...
       ... (posn-y pos) ...
       ... (posn-x manhattan) ...
       ... (posn-y manhattan) ...))
  
(check-expect (manhattan-distance (make-posn 1 2) (make-posn 2 2)) 1)
(check-expect (manhattan-distance (make-posn 2 2) (make-posn 2 4)) 2)
(check-expect (manhattan-distance (make-posn -1 2) (make-posn 2 3)) 4)
(check-expect (manhattan-distance (make-posn 0 0) (make-posn 0 0)) 0)
(define (manhattan-distance Posn manhattan)
  (+ (abs (- (posn-x Posn) (posn-x manhattan))) (abs (- (posn-y Posn) (posn-y manhattan)))))

(define-struct planet [name radius rings])
;; A Planet is a (make-planet String Number Int)
;; where name is the name of a planet in String
;; radius is the radius of a planet in Number
;; rings is the number of a planet has in Intergar
;; and represents a planet's name, radius (in km), and # of rings
;; Example :
(define EARTH (make-planet "Earth" 6371 0))
(define SATURN (make-planet "Saturn" 58232 7))

;; Exercise 3
;; (make-planet name radius rings) : String Number Interger -> Planet
;; (planet-name pt) : Planet -> String
;; extract the first argument in Planet which is a name of a planet
;; (planet-radius (make-planet pt)) : Planet -> Number
;; extract the second argument in Planet which is a radius of a planet
;; (planet-rings (make-planet pt)) : Planet -> Interger
;; extract the third argument in Planet which is the number of rings that a planet has
;; (planet? Any) : Any -> Boolean
;; determine whether the argument is a Planet struct

;; Exercise 4
(define (planet-temp pt)
  (... (planet-name pt) ... (planet-radius pt) ... (planet-rings pt) ... ))

;; Exercise 5
;; ring-discovered : Planet -> String
;; outputs the same planet but with 1 added to its ring count by given a Planet
(check-expect (ring-discovered EARTH) (make-planet "Earth" 6371 1))
(check-expect (ring-discovered SATURN) (make-planet "Saturn" 58232 8))
(define (ring-discovered pt)
  (make-planet (planet-name pt) (planet-radius pt) (+ 1 (planet-rings pt))))

;; Exercise 6
;; bigger-planet : Planet Planet -> String
;; compares two radius of planet and returen the name of the bigger planet
(check-expect (bigger-planet EARTH SATURN) "Saturn")
(check-expect (bigger-planet SATURN EARTH) "Saturn")
(check-expect (bigger-planet SATURN SATURN) "Tied: Saturn - Saturn")
(define (bigger-planet planet1 planet2)
  (cond [(> (planet-radius planet1) (planet-radius planet2)) (planet-name planet1)]
        [(< (planet-radius planet1) (planet-radius planet2)) (planet-name planet2)]
        [(= (planet-radius planet1) (planet-radius planet2))
         (string-append "Tied: " (planet-name planet1) " - " (planet-name planet2))]))