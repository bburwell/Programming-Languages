#lang eopl

; Ben Burwell
; Dr. Kussmaul
; CSI-310 :: Programming Languages
; Homework 6: Eta Conversion

;; ========== PROCEDURE ==========
;; NAME: eta-conv
;; DESC: takes a lambda expression and returns
;;       its eta conversion. If the expression
;;       does not have an eta-conversion, it
;;       returns the expression itself.

(define eta-conv
  (lambda (lst)
    (if ;; check that the parameter is a list of length 3
     (and
      (list? lst)
      (equal? (length lst) 3)
      )
     (if ;; it is a list with the correct length, check
         ;; that it has an eta-conversion
      (and
       (equal? (car lst) 'lambda)
       (list? (cadr lst))
       (equal? (length (cadr lst)) 1)
       (list? (caddr lst))
       (equal? (length (caddr lst)) 2)
       (equal? (caadr lst) (cadr (caddr lst)))
       )
      
      ;; it does, so return the conversion
      (car (caddr lst))
      
      ;; it doesn't, so return the expression
      lst)
     
     ;; not a parseable expression, return it
     lst)))

;; ========== TEST CODE ==========
;; should return a:
(eta-conv '(lambda (x) (a x)))

;; should return (lambda (x) (x a))
(eta-conv '(lambda (x) (x a)))

;; should return ()
(eta-conv '())
