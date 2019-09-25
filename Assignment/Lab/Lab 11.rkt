;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname |Lab 11|) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp") (lib "batch-io.rkt" "teachpack" "2htdp")) #f)))
;; Lab 11
; A KMCA (K-Means Clustering Assignment) is a
; (make-assignment [List-of Centroid] [List-of Nat] Boolean)
(define-struct assignment (centroids labels no-reassignment?))
; - where centroids is the current list of centroids (ordered from 0...k-1)
; - labels are the labels assigned to each datapoint and are centroid indices
;   (example: (list 3 1....) indicates the first datapoint is in the 3rd centroid, and the
;    second datapoint is in the 1st centroid)
; - and no-reassignment? keeps track of whether or not re-assignment has occurred
 
; A Centroid is a (make-centroid Number Number)
(define-struct centroid [x y])
; - where x is the x-coordinate of the centroid
; - and y is the y-coordinate of the centroid
 
; A Datapoint is a (make-datapoint Number Number)
(define-struct datapoint [x y])
; - where x is the x-coordinate of the data point
; - and y is the y-coordinate of the data point

;; ---------------------------------------------------------------------------------------------------
; main : PosInt [List-of Datapoint] -> [List-of [List-of Datapoint]]
; run k-means clustering and output the datapoints binned into their respective clusters
#;(define (main k lop)
  (local
    [; next-assignment/main : KMCA -> KMCA
     ; Advance to the next assignment
     (define (next-assignment/main assignment)
       (next-assignment assignment k lop))
     ; draw/main : KMCA -> KMCA
     ; Draw the current clusters and points
     (define (draw/main assignment)
       (draw assignment k lop))]
    (cluster
     (assignment-labels
      (big-bang (initial-assignment k lop)
                [on-tick next-assignment/main 1]
                [to-draw draw/main]
                [stop-when assignment-no-reassignment?]))
     k lop)))
;; ---------------------------------------------------------------------------------------------------

















