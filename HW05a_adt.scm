#lang eopl

;; HW05a - Environments
;;   Part 2: Abstract Syntax Tree
;;
;; CSI-310 Programming Languages
;; Due: 2013-02-25
;; Name: Ben Burwell


;; helpers and data types
(define scheme-value?
  (lambda (a) #t))

(define-datatype env env?
  (empty-env-record)
  (extended-env-record
   (defs (list-of (list-of scheme-value?)))
   (prev-env env?)))

;; some basics
(define empty-env
  (lambda ()
    (empty-env-record)))

(define extend-env
  (lambda (defs prev-env)
    (extended-env-record defs prev-env)))

;; more helpers
(define def-exists?
  (lambda (sym defs)
    (cond
      [ (equal? (length defs) 0) #f ]
      [ (equal? (caar defs) sym) #t ]
      [ else (def-exists? sym (cdr defs)) ]
      )))

(define get-def
  (lambda (sym defs)
    (cond
      [ (equal? (length defs) 0) 'error ]
      [ (equal? (caar defs) sym) (cadar defs) ]
      [ else (get-def sym (cdr defs)) ]
      )))
  
;; now the fun stuff
(define apply-env
  (lambda (this-env sym)
    (cases env this-env
      (empty-env-record
       ()
       (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record
       (defs prev-env)
       (if (def-exists? sym defs)
           (get-def sym defs)
           (apply-env prev-env sym))))))

;; tests
(apply-env (extend-env '((a 4) (b 5)) (extend-env '((a 1) (b 2) (c 3)) (empty-env))) 'a)