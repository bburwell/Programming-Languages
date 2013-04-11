#lang eopl

;; HW05a - Environments
;;   Part 3: Rib-Cage Implementation
;;
;; CSI-310 Programming Languages
;; Due: 2013-02-25
;; Name: Ben Burwell

;; empty environment
(define empty-env
  (lambda () '(())
    ))

;; extend environment
(define extend-env
  (lambda (sym-val-lst env)
    (cons sym-val-lst env)))

;; helper
(define in-sym-list?
  (lambda (item lst)
    (cond
      [ (equal? (length lst) 0) #f ]
      [ (equal? (caar lst) item) #t ]
      [ else (in-sym-list? item (cdr lst)) ]
      )))

(define get-val
  (lambda (sym lst)
    (cond
      [ (equal? (length lst) 0) 'err ]
      [ (equal? (caar lst) sym) (cadar lst) ]
      [ else (get-val sym (cdr lst)) ]
      )))

;; apply environment
(define apply-env
  (lambda (env sym)
    (cond
      [ (in-sym-list? sym (car env)) (get-val sym (car env)) ]
      [ (equal? (length (car env)) 0) (eopl:error "No binding for" sym) ]
      [ else (apply-env (cdr env) sym) ]
      )))


;; tests
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'a)
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'b)
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'c)