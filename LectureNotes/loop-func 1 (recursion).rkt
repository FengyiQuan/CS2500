;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |loop-func 1|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct pilot [name age])

(define-struct passenger [name seat])
; A passenger is a (make-passenger String String)
; 
(define PASSENGER-1 (make-passenger "nate" "1A"))
(define PASSENGER-2 (make-passenger "boo" "1B"))
(define PASSENGER-3 (make-passenger "mzuck" "30C"))
#;
(define (passenger-temp p)
  ... (passenger-name p) ... (passenger-seat p) ...)
  
(define-struct flight [passenger flight])

; A Flight is one of:
; - #false
; - (make-flight Passenger Flight)
; Examples:
(define FLIGHT-0 #false)
(define FLIGHT-1 (make-flight PASSENGER-3 FLIGHT-0))
(define FLIGHT-2 (make-flight PASSENGER-1 (make-flight PASSENGER-2 FLIGHT-1)))

(define (flight-temp f)
  (cond
    [(boolean? f) ...]
    [(flight? f) ...
     (passenger-temp (flight-passenger f)) ...
     (flight-temp (flight-flight f)) ...]))
  


