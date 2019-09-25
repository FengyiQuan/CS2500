;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname sample03) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

;; A CryptoName is one of:
;; - "Alice"
;; - "Bob"
;; - "Carol"
;; and represents the collection of names used in crypotography narratives

;; Exercise 1

(define ALICE "Alice")
(define BOB "Bob")
(define CAROL "Carol")

;; Exercise 2

;; cryptoname-temp : CryptoName -> ?
(define (cryptoname-temp cn)
  (cond [(string=? ALICE cn) ...]
        [(string=? BOB cn) ...]
        [(string=? CAROL cn) ...]))

;; Exercise 3

;; associated-cryptoname : CryptoName KeyEvent -> CryptoName
;; The associated crypotname of the key event (based on first letter), else backup
(check-expect (associated-cryptoname BOB "a") ALICE)
(check-expect (associated-cryptoname CAROL "b") BOB)
(check-expect (associated-cryptoname ALICE "c") CAROL)
(check-expect (associated-cryptoname BOB "d") BOB)
(define (associated-cryptoname backup ke)
  (cond [(key=? "a" ke) ALICE]
        [(key=? "b" ke) BOB]
        [(key=? "c" ke) CAROL]
        [else backup]))

;; Exercise 4

;; next-or-associated : CryptoName KeyEvent -> CryptoName
;; The next cryptoname if ke is "right", or the associated crytponame (if applicable)
(check-expect (next-or-associated ALICE "right") BOB)
(check-expect (next-or-associated ALICE "b") BOB)
(check-expect (next-or-associated ALICE "d") ALICE)
(define (next-or-associated cn ke)
  (cond [(key=? ke "right") (next cn)]
        [else (associated-cryptoname cn ke)]))

;; next : CryptoName -> CryptoName
;; Output the next crytponame
(check-expect (next ALICE) BOB)
(check-expect (next BOB) CAROL)
(check-expect (next CAROL) ALICE)
(define (next cn)
  (cond [(string=? ALICE cn) BOB]
        [(string=? BOB cn) CAROL]
        [(string=? CAROL cn) ALICE]))

;; Exercise 5

(define BG (empty-scene 200 200))
(define FONT-COLOR "black")
(define FONT-SIZE 20)

;; draw-cryptoname : CryptoName -> Image
;; Draw the cryptoname
(check-expect (draw-cryptoname ALICE) (overlay (text "Alice" FONT-SIZE FONT-COLOR) BG))
(define (draw-cryptoname cn)
  (overlay (text cn FONT-SIZE FONT-COLOR) BG))

;; Exercise 6 
;; main/crypto : CryptoName -> CryptoName
;; Launch the crypto name program which lets people cycle through cryptonames or jump to them
(define (main/crypto cn)
  (big-bang cn
    [to-draw draw-cryptoname]
    [on-key next-or-associated]))