#lang scheme

; ------------------------------------------------------------
; PROGRAM NAME: HW02.rkt
;
; DESCRIPTION: Second homework assignment for CSI310
;
; AUTHOR: Ben Burwell ; CS 310: Theory of Programming Languages
; Muhlenberg College
;

;; ---------
;; NAME: invert
;; DESC: takes a list of 2-lists (lists of length 2), and returns a list with each 2-list reversed.

(define invert
  (lambda (l)
    (map swap l)))

(define swap
  (lambda (l)
    (if (= 2 (length l))
     (list (cadr l) (car l))
     (display "Error"))))

;; TEST
(invert '((2 1) (4 3) (6 5))) ;; should return (1 2) (3 4) (5 6)


;; ---------
;; NAME: compose23
;; DESC: that takes 1, 2, or 3 procedures and composes them, as specified by the equation:
;;           (compose f g h) = (compose f (compose g h))

(define compose23
  (lambda funcs
    (if (= (length funcs) 1)
     (car funcs)
     (if (= (length funcs) 2)
         ((car funcs) (cadr funcs))
         (if (= (length funcs) 3)
         ((car funcs (cadr funcs) (caddr funcs))))))))