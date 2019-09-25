;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname template-temperature) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A Temperature is a real number in [0, 10e32)
;; Interpretation: degrees Kelvin
;;Examples:
(define Temperature-0 0)
(define Temperature-20 20)

(define (temperature-temp t)  ;temptation
  ... t ...)





;; An MBTANonBusLine is one of:
;; - "Orange"
;; - "Red"
;; - "Green"
;; - "Blue"
;; - "Commuter Rail"
;; Interpretation: a train route for the MBTA
;; Examples:
(define MBTA-Orange "Orange")
(define MBTA-Red "Red")
(define MBTAe-Green "Green")

(define (mbta-temp mbta)
  (cond
    [(string=? mbta MBTA-RED) ...]
    []
    []
    []
    []))

