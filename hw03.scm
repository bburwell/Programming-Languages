#lang scheme
;; NAME: hw03.scm
;; AUTHOR: Ben Burwell
;; DESC: CSI310 - Programming Languages - Homework 3
;; HISTORY: Created 2013-01-31

;; ========== PROCEDURE ==========
;; NAME: merge
;; DESC: returns a sorted list of all of the numbers in
;;       the two parameters
(define merge
  (λ (lon1 lon2)
    (qsort (append lon1 lon2))
    ))

;; helper function
;; this is an implementation of quicksort
(define qsp-helper
  (λ (all check less more)
    (cond
      [ (null? all) (cons less (cons check (cons more '()))) ]
      [ else (let ((x (car all)))
               (if (<= x check)
                   (qsp-helper (cdr all) check (cons x less) more)
                   (qsp-helper (cdr all) check less (cons x more)))) ]
      )))

(define qspartition
  (λ (lst)
    (qsp-helper (cdr lst) (car lst) '() '())
    ))

(define qsort
  (λ (lst)
    (cond
      [ (null? lst) lst ]
      [ else (let ((list1 (qspartition lst)))
               (append
                (qsort (car list1))
                (cons
                 (cadr list1)
                 (qsort (caddr list1)))
                )) ]
      )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (merge) ==========================================================================")
(newline)

(display "Expected output: (1 2 3 4 5 6)                        ")
(merge '(1 3 5) '(2 4 6))

(display "Expected output: (1 2 2 3)                            ")
(merge '(1 2) '(2 3))

;; ========== PROCEDURE ==========
;; NAME: car&cdr
;; DESC: returns the code for a procedure 
;;       that takes a list with the same structure 
;;       as slst and returns the value in the 
;;       same position as the leftmost occurrence 
;;       of s in slst. If s does not occur in 
;;       slst then errval is returned.
(define car&cdr-help
  (λ (s slst errval var)
    (cond
      [ (null? slst) errval ]
      [ (equal? (car slst) s) (list 'λ '(lst) (list 'car var)) ]
      [ (and (list? (car slst)) (car&cdr-help s (car slst) errval (list 'car var))) ]
      [ (car&cdr-help s (cdr slst) errval (list 'cdr var)) ]
      [ else errval ]
      )))

(define car&cdr 
    (λ (s slst errval)
      (cond
        [ (not (list? slst)) (display "Not a list") ]
        [ (list? s) (display "Cannot find a list") ]
        [ else (car&cdr-help s slst errval 'lst) ]
      )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (car&cdr) ========================================================================")
(newline)

(display "Expected output: (λ (lst) (car (cdr (cdr lst))))      ")
(car&cdr 'c '(a b c d) 'fail)

(display "Expected output: pass                                 ")
(car&cdr 'a '(b c d) 'pass)




;; ========== PROCEDURE ==========
;; NAME: if->cond
;; DESC: that takes an if expression and 
;;       returns the corresponding cond 
;;       expression.

(define if->cond
  (λ (condlist)
    (cond
      [ (equal? 3 (length condlist)) (list 'cond (list 'else (list 'if (cadr condlist) (caddr condlist)))) ]
      [ (list? (cadddr condlist)) (cons 'cond (ifcondhelper condlist)) ]
      [ else (list 'cond (list (cadr condlist) (caddr condlist)) (list 'else (cadddr condlist))) ]
    )))

(define ifcondhelper
  (λ (condlist)
    (cond
      [ (list? (cadddr condlist)) (cons (list (cadr condlist) (caddr condlist)) (ifcondhelper (cadddr condlist))) ]
      [ else (list (list (cadr condlist) (caddr condlist)) (list 'else (cadddr condlist))) ]
    )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (if->cond) =======================================================================")
(newline)

(display "Expected output: (cond (else (if a b)))               ")
(if->cond '(if a b))

(display "Expected output: (cond (a b) (else c))                ")
(if->cond '(if a b c))

(display "Expected output: (cond (a b) (c d) (else e))          ")
(if->cond '(if a b (if c d e)))

(display "Expected output: (cond (a (if b c d)) (e f) (else g)) ")
(if->cond '(if a (if b c d) (if e f g)))