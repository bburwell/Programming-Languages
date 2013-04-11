#lang scheme

;; circlearea
;;
;; find the area of a circle with radius r
(define
  (circlearea r)
  (* 3.142 (expt r 2))
)

;; between?
;;
;; check whether a is between x and y
(define
  (between? a x y)
  (if
   (or
    (and (< y a) (> x a)) ;; y < a < x
    (and (> y a) (< x a)) ;; x < a < y
   )
   #t
   #f
  )
)

;; shorter
;;
;; returns the list with fewer elements
(define
  (shorter a b)
  (if
   (< (length a) (length b))
   a
   b
 )
)

;; righttri?
;;
;; checks whether 3 integers can be the side lengths
;; of a right triangle.
(define
  (righttri? a b c)
  (cond
    ;; a is largest (b^2 + c^2 = a^2)
    [ (and (> a b) (> a c)) (= (+ (expt b 2) (expt c 2)) (expt a 2)) ]
    
    ;; b is largest (a^2 + c^2 = b^2)
    [ (and (> b a) (> b c)) (= (+ (expt a 2) (expt c 2)) (expt b 2)) ]
    
    ;; c is largest (a^2 + b^2 = c^2)
    [ (and (> c a) (> c b)) (= (+ (expt a 2) (expt b 2)) (expt c 2)) ]
  )
)