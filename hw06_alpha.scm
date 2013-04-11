#lang eopl

; Ben Burwell
; Dr. Kussmaul
; CSI-310 :: Programming Languages
; Homework 6: Alpha conversions

;; ========== PROCEDURE ==========
;; NAME: alpha-conv
;; DESC: returns the alpha conversion of an expression if
;;       it exists. If there is no alpha conversion, the
;;       expression itself is returned.
(define alpha-conv
  (lambda (from to exp)
    (if
     (list? exp)
     (if
      (equal? (length exp) 3)
      (if
       (and
        (equal? (car exp) 'lambda)
        (list? (cadr exp))
        (equal? (length (cadr exp)) 1)
        (list? (caddr exp))
        )
       (list 'lambda (list to) (list-repl from to (caddr exp)))
       exp)
      exp)
     exp)))

;; ========== PROCEDURE ==========
;; NAME: list-repl
;; DESC: a helper function for the alpha conversion
;;       replaces all occurrences of from with to in
;;       exp unless there is a nested lambda function,
;;       in which case it remains intact.
(define list-repl
  (lambda (from to exp)
    (cond
      [ (equal? (length exp) 0) '() ]
      [ (equal? (car exp) from) (cons to (list-repl from to (cdr exp))) ]
      [ (and (list? (car exp)) (equal? (caar exp) 'lambda)) (cons (car exp) (list-repl from to (cdr exp))) ]
      [ (list? (car exp)) (cons (list-repl from to (car exp)) (list-repl from to (cdr exp))) ]
      [ else (cons (car exp) (list-repl from to (cdr exp))) ]
      )))

;; ========== TEST CODE ==========
(alpha-conv 'x 'y '(x z))
(alpha-conv 'x 'y '((lambda ( x ) (+ x 5)) 2))
(alpha-conv 'x 'y '(lambda ( x ) (x z x)))
(alpha-conv 'x 'y '(lambda ( y ) (y z y)))
(alpha-conv 'x 'y '(lambda ( x ) (+ x ((lambda (x) (* 2 x)) 7))))