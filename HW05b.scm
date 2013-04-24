#lang eopl

(define scanner-spec
  '(
    (whitespace (whitespace) skip)
    (number (digit (arbno digit)) make-number)
    (operator ((or "+" "-" "*" "/" "(" ")")) make-symbol)))

(define grammar
  '(
    (expression (term (arbno add-op term)) arith-expr)
    (term (factor (arbno mult-op factor)) arith-factor)
    (factor (number) number-factor)
    (factor ("(" expression ")") expr-factor)
    (add-op ("+") plus-op)
    (add-op ("-") minus-op)
    (mult-op ("*") times-op)
    (mult-op ("/") divide-op)
    ))

(sllgen:make-define-datatypes scanner-spec grammar)
(define dump-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec grammar)))


(define execute-expression
  (lambda (expr)
    (cases expression expr
      (arith-expr (term ops terms) (expr-helper term ops terms))
      )))

(define expr-helper
  (lambda (term ops terms)
    (if (null? ops)
        (eval-term term)
        (if (equal? (length ops) 1)
            (eval-add-op (car ops) term (car terms))
            
            'a
            
            ))))

(define eval-add-op
  (lambda (op term1 term2)
    (cases add-op op
      (plus-op () (+ (eval-term term1) (eval-term term2)))
      (minus-op () (- (eval-term term1) (eval-term term2)))
      )))

(define eval-term
  (lambda (term1)
    (cases term term1
      (arith-factor (factor ops factors) (term-helper factor ops factors))
      )))

(define term-helper
  (lambda (factor ops factors)
    (if (null? ops)
        (eval-factor factor)
        (if (equal? (length ops) 1)
            (eval-mult-op (car ops) factor (car factors))
            
            'b
            
            ))))

(define eval-mult-op
  (lambda (op fac1 fac2)
    (cases mult-op op
      (times-op () (* (eval-factor fac1) (eval-factor fac2)))
      (divide-op () (/ (eval-factor fac1) (eval-factor fac2)))
      )))

(define eval-factor
  (lambda (fac)
    (cases factor fac
      (number-factor (num) num)
      (expr-factor (expr) (execute-expression expr))
      )))
  

(define scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define scan&parse
  (sllgen:make-string-parser scanner-spec grammar))
(define read-dump
  (sllgen:make-rep-loop "> " (lambda (tree) tree) (sllgen:make-stream-parser scanner-spec grammar)))

(define scan&parse&eval
  (lambda (s) (execute-expression (scan&parse s))))

(define read-eval-print
  (sllgen:make-rep-loop "> " execute-expression (sllgen:make-stream-parser scanner-spec grammar)))

(define spe scan&parse&eval)