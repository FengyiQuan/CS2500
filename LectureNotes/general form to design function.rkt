;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |general form to design function|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; 1. Signature
;; 2. Purpose
;; 3. tests
;; 4. Function

(define (gen-rec prob)
  (cond
    [(trivial1? prob) ...]
    [(trivial2? prob) ...]
    ...
    [(non-trivial1? prob) ...
     (combine
      (gen-rec (transform1 prob))
      (gen-rec (transform2 prob)))]))

;; 5. Termination