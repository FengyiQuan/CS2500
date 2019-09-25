;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname PosnOrString) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; A PosnOrString is one of:
;; - (make-posn RealNumber RealNumber)
;; - String
;; Interpretation: map coordinate or nickname
;; Example:
(define POSN-Or-STRING_1 (make-posn 10 10))
(define POSN-Or-STRING_2 "name")

#;
(define (posn-or-string-temp pos)
  (cond
    [(posn? pos) ... (posn-x pos) ... (posn-y pos) ...]
    [(string? pos) (... pos ...)]))


;; An NUID is one of:
;; - #false
;; - PosInteger

;; Interpretation: unique student identifier, or #false if unknown
;; Example:
(define NUID-1 456)
(define NUID-2 #false)

#;
(define (nuid-temp nuid)
  (cond [(boolean? nuid) ...]
        [(integer? nuid) ... nuid ...]))





