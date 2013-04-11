#lang eopl

;; HW05a - Environments
;;   Part 1: Procedural Implementation
;;
;; CSI-310 Programming Languages
;; Due: 2013-02-25
;; Name: Ben Burwell

;; empty environment
(define empty-env
  (lambda ()
    (lambda (sym) (eopl:error 'apply-env "No binding for ~s" sym))))

;; helpers
(define in-sym-list
  (lambda (item lst)
    (cond
      [ (equal? (length lst) 0) #f ]
      [ (equal? (caar lst) item) #t ]
      [ else (in-sym-list item (cdr lst)) ]
      )))

(define get-val
  (lambda (sym lst)
    (cond
      [ (equal? (length lst) 0) 'err ]
      [ (equal? (caar lst) sym) (cadar lst) ]
      [ else (get-val sym (cdr lst)) ]
      )))

;; extend environment
(define extend-env
  (lambda (sym-val-lst env)
    (lambda (sym)
       (if (in-sym-list sym sym-val-lst)
           (get-val sym sym-val-lst)
           (apply-env env sym)))))

;; apply environment
(define apply-env
  (lambda (env sym)
    (env sym)))

;; tests
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'a)
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'b)
(apply-env (extend-env '((a 5) (b 9)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'c)