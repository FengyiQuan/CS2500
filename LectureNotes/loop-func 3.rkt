;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname |loop-func 3|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)
(define-struct moon [name cheese])
; A Moon is a (make-moon String String)

(define MOON-1 (make-moon "the moon" "american"))
(define MOON-2 (make-moon "titan" "cheese"))
(define MOON-3 (make-moon "io" "string"))

#;
(define (moon-temp m)
  ... (moon-name m) ... (moon-cheese m) ...)

(define-struct system [moon system])
; A system is one of :
; - (make-system Moon System)
; #false
;第一个字母大写data  type
(define SYSTEM-0 #false)
(define SYSTEM-1 (make-system MOON-1 SYSTEM-0))
(define SYSTEM-3 (make-system MOON-3
                              (make-system MOON-2 SYSTEM-1)))
;(define (system-temp s)
;  (cond
;    [(boolean? s) ...]
;    [(system? s) ...
;     (system-temp (system-system s)) ...]))


(check-expect (all-the-cheese SYSTEM-3) "americancheesestring")

(define (all-the-cheese s)
  (cond
    [(boolean? s) ""]
    [(system? s) (string-append
                  (system-moon(all-the-cheese (system-system s)))
                  (system-moon s))]))




