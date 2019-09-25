;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname lab11-implementation) (read-case-sensitive #t) (teachpacks ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp"))) (htdp-settings #(#t constructor repeating-decimal #f #t none #f ((lib "image.rkt" "teachpack" "2htdp") (lib "universe.rkt" "teachpack" "2htdp")) #f)))
; A KMCA (K-Means Clustering Assignment) is a
; (make-assignment [List-of Centroid] [List-of Nat] Boolean)
(define-struct assignment (centroids labels no-reassignment?))
; - where centroids is the current list of centroids (ordered from 0...k-1)
; - labels are the labels assigned to each point
;   (the first point is labeled the first element in labels, second point the second label, etc.)
; - and any-assignment? keeps track of whether or not re-assignment has occurred

;; A Centroid is a (make-centroid Number Number)
(define-struct centroid [x y])
;; - where x is the x-coordinate of the centroid
;; - and y is the y-coordinate of the centroid

;; A Datapoint is a (make-datapoint Number Number)
(define-struct datapoint [x y])
;; - where x is the x-coordinate of the data point
;; - and y is the y-coordinate fo the data point

; main : PosInt [List-of Datapoint] -> [List-of [List-of Datapoint]]
; run k-means clustering and output the datapoints binned into their respective clusters
(define (main k lop)
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

;; first-n : Nat [List-of X] -> [List-of X]
;; first n in lox
(check-expect (first-n 3 (list "a" "b" "c" "d" "e")) (list "a" "b" "c"))
(define (first-n n lox)
  (if (zero? n) '() (cons (first lox) (first-n (sub1 n) (rest lox)))))

;; distance : Datapoint Centroid -> Number
;; Distance between a datapoint and a centroid
(check-expect (distance (make-datapoint 0 0) (make-centroid 3 4)) 5)
(define (distance dp ctr)
  (sqrt (+ (sqr (- (datapoint-x dp) (centroid-x ctr)))
           (sqr (- (datapoint-y dp) (centroid-y ctr))))))

;; datapoint-mean : [List-of Datapoint] -> Centroid
;; Computes the mean of a list of data points
(check-expect
 (datapoint-mean (list (make-datapoint 0 0) (make-datapoint 3 4)))
 (make-centroid 1.5 2))
(define (datapoint-mean lop)
  (make-centroid (/ (foldr + 0 (map datapoint-x lop)) (length lop))
                 (/ (foldr + 0 (map datapoint-y lop)) (length lop))))

;; datapoint-list-mean : [List-of [List-of Datapoint]] -> [List-of Centroid]
;; Computes the mean of each list of datapoints
(check-expect
 (datapoint-list-mean (list (list (make-datapoint 0 0) (make-datapoint 3 4))))
 (list (make-centroid 1.5 2)))
(define (datapoint-list-mean lolodp)
  (map datapoint-mean lolodp))

;; cluster : [List-of Nat] Nat [List-of X] -> [List-of [List-of X]]
;; Given a list of labels and the number of clusters, cluster lox
;; (length labels) = (length lox)
(check-expect (cluster (list 0 1 3 0 1 3)
                       4
                       (list "a" "b" "c" "d" "e" "f"))
              (list (list "a" "d")
                    (list "b" "e")
                    '()
                    (list "c" "f")))
(define (cluster labels k lox)
  (local [;; cons-at-i : Nat X [List-of [List-of X]] -> [List-of [List-of X]]
          ;; cons x at i
          (define (cons-at-i i x lolox)
            (cond [(zero? i) (cons (cons x (first lolox)) (rest lolox))]
                  [else (cons (first lolox) (cons-at-i (sub1 i) x (rest lolox)))]))]
    (foldr cons-at-i (make-list k '()) labels lox)))

;; assign-new-labels : [List-of Centroid] Nat [List-of Datapoint]  -> [List-of Nat]
;; Given the current centroids and the list of data points, output the new labels
(check-expect (assign-new-labels (list (make-centroid -5 -5) (make-centroid 5 5))
                                 2
                                 (list (make-datapoint -20 -20)
                                       (make-datapoint -3 1)
                                       (make-datapoint 20 20)))
              (list 0 0 1))
(define (assign-new-labels centroids k lodp)
  (local [;; label : Datapoint -> Nat
          ;; Output the label for the given data point
          (define (label dp)
            (local [;; distance-to-centroid : Nat -> Number
                    ;; Get the distance from dp to the centroid at i
                    (define (distance-to-centroid i)
                      (distance dp (list-ref centroids i)))]
              (argmin distance-to-centroid (build-list k identity))))]
    (map label lodp)))

;; initial-assignment : PosInt [List-of Datapoint] -> KMCA
;; The initial assignments
(check-expect (initial-assignment 2 (list (make-datapoint 0 0)
                                          (make-datapoint 1 1)
                                          (make-datapoint 20 20)))
              (make-assignment (list (make-centroid 0 0)
                                     (make-centroid 1 1))
                               (list 0 1 1)
                               #f))
(define (initial-assignment k lodp)
  (local [(define centroids (map datapoint->centroid (first-n k lodp)))
          (define labels (assign-new-labels centroids k lodp))]
    (make-assignment centroids labels #f)))

;; datapoint->centroid : Datapoint -> Centroid
;; Convert from a data point to a centroid
(check-expect (datapoint->centroid (make-datapoint 0 0)) (make-centroid 0 0))
(check-expect (datapoint->centroid (make-datapoint 1 2)) (make-centroid 1 2))
(define (datapoint->centroid dp)
  (make-centroid (datapoint-x dp) (datapoint-y dp)))

;; next-assignment : KMCA PosInt [List-of Datapoint] -> KMCA
;; The next assignment
(check-expect (next-assignment
               (make-assignment (list (make-centroid 0 0)
                                      (make-centroid 1 1))
                                (list 0 1 1)
                                #f)
               2
               (list (make-datapoint 0 0)
                     (make-datapoint 1 1)
                     (make-datapoint 20 20)))
              (make-assignment (list (make-centroid 0 0)
                                     (make-centroid 21/2 21/2))
                               (list 0 0 1)
                               #f))
(define (next-assignment assignment k lodp)
  (local [(define binned (cluster (assignment-labels assignment) k lodp))
          (define centroids (datapoint-list-mean binned))
          (define labels (assign-new-labels centroids k lodp))]
    (make-assignment centroids labels (equal? labels (assignment-labels assignment)))))

(define BG (empty-scene 500 500))
(define COLORS (list "red" "purple" "blue" "green" "yellow" "orange" "pink" "grey"))

;; draw-centroid : Centroid Color Image -> Image
;; Draw a centroid on the given image
(check-expect
 (draw-centroid (make-centroid 0 0) "red" (circle 10 "solid" "black"))
 (place-image (text "X" 20 "red") 0 0 (circle 10 "solid" "black")))
(define (draw-centroid ctr color bg)
  (place-image (text "X" 20 color) (centroid-x ctr) (centroid-y ctr) bg))

;; draw-point : Datapoint Color Image -> Image
;; Draw a datapoint onto the given image
(check-expect
 (draw-point (make-datapoint 10 2) "red" (circle 10 "solid" "black"))
 (place-image (circle 3 "outline" "red") 10 2 (circle 10 "solid" "black")))
(define (draw-point dp color bg)
  (place-image (circle 3 "outline" color) (datapoint-x dp) (datapoint-y dp) bg))

;; draw-shapes : [List-of X] [List-of Nat] [X Color Image -> Image] Image -> Image
;; Draw shapes on i at lop using colors from lon
(check-expect (draw-shapes (list (make-centroid 0 0) (make-centroid 1 1))
                           (list 0 3)
                           draw-centroid
                           BG)
              (draw-centroid (make-centroid 0 0) (first COLORS)
                             (draw-centroid (make-centroid 1 1) (third COLORS)
                                            BG)))
(define (draw-shapes xs labels x->image bg)
  (local [;; draw-shape : X Nat Image -> Image
          ;; Draw using the x->i function and the given color
          (define (draw-shape x color-index bgimg)
            (x->image x (list-ref COLORS color-index) bgimg))]
    (foldr draw-shape bg xs labels)))

;; draw-data : [List-of Datapoint] [List-of Nat] Image -> Image
;; Draw data points using colors in lon on i
(check-expect (draw-data (list (make-datapoint 0 0)
                               (make-datapoint 1 1)
                               (make-datapoint 2 2))
                         (list 2 1 0)
                         BG)
              (draw-point (make-datapoint 0 0) (third COLORS)
                          (draw-point (make-datapoint 1 1) (second COLORS)
                                      (draw-point (make-datapoint 2 2) (first COLORS)
                                                  BG))))
(define (draw-data lodp lon bg)
  (draw-shapes lodp lon draw-point bg))

;; draw-centroids : [List-of Centroid] Nat Image -> Image
;; Draw centroids on i
(check-expect (draw-centroids (list (make-centroid 0 0)
                                    (make-centroid 1 1)
                                    (make-centroid 2 2))
                              3
                              BG)
              (draw-centroid (make-centroid 0 0) (first COLORS)
                             (draw-centroid (make-centroid 1 1) (second COLORS)
                                            (draw-centroid (make-centroid 2 2) (third COLORS)
                                                           BG))))
(define (draw-centroids loc k bg)
  (draw-shapes loc (build-list k identity) draw-centroid bg))

;; draw : KMCA K [List-of Datapoint] -> Image
;; Draw the assignment
(check-expect (draw (make-assignment (list (make-centroid 5 5)) (list 0) #f)
                    1
                    (list (make-datapoint 10 10)))
              (draw-centroid (make-centroid 5 5) (first COLORS)
                             (draw-point (make-datapoint 10 10) (first COLORS)                                         
                                         BG)))
(define (draw assignment k lodp)
  (draw-centroids (assignment-centroids assignment) k
                  (draw-data lodp (assignment-labels assignment) BG)))

; random-datapoint : ? -> Datapoint
; Make a random datapoint
(define (random-datapoint _)
  (make-datapoint (random 500) (random 500)))
(define DATAPOINTS (build-list 50 random-datapoint))
(main 4 DATAPOINTS)
