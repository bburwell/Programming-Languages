#lang eopl

;; scanner spec
(define scanner-spec
  '(
    (whitespace (whitespace) skip)
    (number (digit (arbno digit)) make-number)
    (operator ((or "+"  "-"  "*"  "/"  "("  ")")) make-symbol)))

;; grammar
(define grammar
  '(
    ;; expression - 2+4*2*(1+2+3)
    (expression (term (arbno add-op term)) arith-expr)
    
    ;; term - 1*2*3
    (term (factor (arbno mult-op factor)) arith-factor)
    
    ;; factors - 2, (1+2+3)
    (factor (number) number-factor)
    (factor ("(" expression ")") expr-factor)
    
    ;; addition and subtraction
    (add-op ("+") plus-op)
    (add-op ("-") minus-op)
    
    ;; multiplication and division
    (mult-op ("*") times-op)
    (mult-op ("/") divide-op)
    ))

;; make the datatypes
(sllgen:make-define-datatypes scanner-spec grammar)
(define dump-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec grammar)))

;; top level expression evaluator
(define execute-expression
  (lambda (expr)
    (cases expression expr
      ;; use a helper function to keep code out of here
      (arith-expr (term ops terms) (expr-helper term ops terms))
      )))

;; helper function to evaluate an expression
(define expr-helper
  (lambda (term ops terms)
    (if (null? ops)
        ;; single term
        (eval-term term)
        
        ;; else, check if there's just one term
        (if (equal? (length ops) 1)
            (eval-add-op (car ops) term (car terms))
            
            ;; else, handle multiple terms
            'a
            
            ))))

;; evaluates an add operator
(define eval-add-op
  (lambda (op term1 term2)
    (cases add-op op
      
      ;; perform addition
      (plus-op () (+ (eval-term term1) (eval-term term2)))
      
      ;; perform subtraction
      (minus-op () (- (eval-term term1) (eval-term term2)))
      )))

;; evaluate a term
(define eval-term
  (lambda (term1)
    (cases term term1
      
      ;; use a helper function to keep this one clean
      (arith-factor (factor ops factors) (term-helper factor ops factors))
      )))

;; helper function for evaluating terms
(define term-helper
  (lambda (factor ops factors)
    
    ;; if there is a single factor...
    (if (null? ops)
        ;; ... evaluate and return it
        (eval-factor factor)
        
        ;; otherwise, if there are 2 factors...
        (if (equal? (length ops) 1)
            
            ;; evaluate and return them.
            (eval-mult-op (car ops) factor (car factors))
            
            ;; otherwise, handle multiple factors
            'b
            
            ))))

;; handles multiplication and division
(define eval-mult-op
  (lambda (op fac1 fac2)
    (cases mult-op op
      
      ;; perform multiplication
      (times-op () (* (eval-factor fac1) (eval-factor fac2)))
      
      ;; perform division
      (divide-op () (/ (eval-factor fac1) (eval-factor fac2)))
      )))

;; evaluate a factor
(define eval-factor
  (lambda (fac)
    (cases factor fac
      
      ;; if it's a number, give the number
      (number-factor (num) num)
      
      ;; if it's an expression, evaluate the expression
      (expr-factor (expr) (execute-expression expr))
      )))
  
;; SLLGEN things
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

;; shortcut for quicker testing
(define spe scan&parse&eval)