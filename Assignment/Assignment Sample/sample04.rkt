;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample04) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
;; Exercise 1

;; A MusicPlayer is a (make-player String String String)
(define-struct player [current-song next-song feedback])
;; and represents a the encoding of the current song, the next song to be played,
;; and the feedback given of the current song

(define PLAYER-EXAMPLE (make-player "" "" "I liked it"))

;; player-temp : MusicPlayer -> ?
(define (player-temp mp)
  (... (player-current-song mp)
       (player-next-song mp)
       (player-feedback mp)))

;; Exercise 2

;; manhattan-distance : Posn Posn -> Number
;; Manhattan distance
(check-expect (manhattan-distance (make-posn 10 20)
                                  (make-posn 5 30))
              15)
(define (manhattan-distance p1 p2)
  (+ (abs (- (posn-x p1) (posn-x p2)))
     (abs (- (posn-y p1) (posn-y p2)))))

;; Exercise 3

(define-struct planet [name radius rings])
; An Planet is a (make-planet String Number Int)
; and represents a planet's name, radius (in km), and # of rings
 
(define EARTH (make-planet "Earth" 6371 0))
(define SATURN (make-planet "Saturn" 58232 7))

;; make-planet : String Number Int -> Planet
;; planet? : Any -> Boolean
;; planet-name : Planet -> String
;; planet-rad : Planet -> Number
;; planet-rings : Planet -> Int

;; Exercise 4

;; planet-temp : Planet -> ?
(define (planet-temp p)
  (... (planet-name p) (planet-radius p) (planet-rings p)))

;; Exercise 5

;; ring-discovered : Planet -> Planet
;; add1 to the planet's rings
(check-expect (ring-discovered SATURN) (make-planet "Saturn" 58232 8))
(define (ring-discovered p)
  (make-planet (planet-name p) (planet-radius p) (add1 (planet-rings p))))

;; Exercise 6

;; bigger-planet : Planet Planet -> String
;; The name of the bigger planet, or show a tie occurred
(check-expect (bigger-planet EARTH SATURN) "Saturn")
(check-expect (bigger-planet SATURN EARTH) "Saturn")
(check-expect (bigger-planet SATURN SATURN) "Tied: Saturn - Saturn")
(define (bigger-planet p1 p2)
  (cond [(> (planet-radius p1) (planet-radius p2)) (planet-name p1)]
        [(< (planet-radius p1) (planet-radius p2)) (planet-name p2)]
        [else (string-append "Tied: " (planet-name p1) " - " (planet-name p2))]))