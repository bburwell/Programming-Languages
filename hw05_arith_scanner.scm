#lang eopl

;; <arith-expr> ::== <arith-term> { <add-op> <arith-term> }*
;; <arith-term> ::== <arith-factor> { <mult-op> <arith-factor> }*
;; <arith-factor> ::== <number>
;; <arith-factor> ::== ( <arith-expr> )
;; <add-op> ::== + | -
;; <mult-op> ::== * | / 

(define arith-scanner
  '(
    (add-op (or "+" "-") symbol)
    (mult-op (or "*" "/") symbol)
    (number (digit (arbno digit)) number)
  ))

(define arith-grammar
  '(
    (expression (number) arith-factor)
    ;(expression (number add-op number) arith-factor)
    ;(expression (expression add-op expression) arith-factor)
    ))

(sllgen:make-define-datatypes arith-scanner arith-grammar)

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes arith-scanner arith-grammar)))

(define just-scan
  (sllgen:make-string-scanner arith-scanner '()))

(define scan&parse
  (sllgen:make-string-parser arith-scanner arith-grammar) )

(define read-eval-print
  (sllgen:make-rep-loop "--> " stmt-evaluator
                        (sllgen:make-stream-parser arith-scanner arith-grammar) ))