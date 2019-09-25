;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname string-label) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; Given a list of strings and a natural number n, generate a list of labels,
; as follows. For each string s in the list, the labels are the strings s1 ... sn.

; labels: [List-of String] Number -> [List-of String]
; Given (a b ... z) and n , produces (a1 ... an b1 ... bn ... z1 ... zn)
(check-expect (labels '() 2) '())
(check-expect (labels (list "a" "b" "c") 2) (list "a1" "a2" "b1" "b2" "c1" "c2"))
(define (labels los n)
  (local [ ; add-labels : String [List-of String] -> [List-of String]
	   ; adds labels of given string to given list
	   ; given "a" and (list "b1" "b2" "c1" "c2) return (list "a1" "a2" "b1" "b2" "c1" "c2")
	   (define (add-labels s rest)
	     (append (make-labels s) rest))
	   ; make-labels : String -> [List-of String]
	   ; make the labels for the given string
	   ; with n=2, given "a" return (list "a1" "a2")
	   (define (make-labels s)
	     (local [ ; num2str : Number -> String
		      ; prefixes given number with string s
		      ; with s="a", given 3 return "a3"
		      (define (num2str i)
			(string-append s (number->string i)))]
               (map num2str (build-list n add1))))]
    ; foldr: (X Y) [X Y -> Y] Y [List-of X] -> Y
    ; X = String , Y = [List-of String]
    (foldr add-labels '() los)))
