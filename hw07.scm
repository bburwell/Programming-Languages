#lang eopl

; 3.4 INTERPRETER
; - this interpreter consists of several components
;  - SCANNER    - converts characters to tokens
;  - PARSER      - converts tokens to abstract syntax trees
;  - ENVIRONMENT - maps identifiers to values
;  - EVALUATOR  - converts abstract syntax trees to values/results
; - this interpreter supports:
;  - (3.1) basic functionality
;    - whitespace & comments (skipped)
;    - integers & built-in defined constants
;    - 3 binary primitive operations - add, sub, mult
;    - 2  unary primitive operations - incr, decr
;  - (3.3) conditionals  - if-then-else
;  - (3.4) local bindings - let 
;
; - things to do (save as 3.4a)
;

; ------------------------------------------------------------
; scanner specification

(define scanner-spec
  '(
    (whitespace (whitespace)                          skip)
    (comment    ("%" (arbno (not #\newline)))          skip)
    (identifier (letter (arbno (or letter digit "?"))) symbol)
    (number    (digit (arbno digit))                  number) ))

; ------------------------------------------------------------
; grammar specification

(define grammar
  '(
    ; (3.1) program
    (program    (expression)
                a-program)
    ; (3.1) expressions
    (expression (number)
                lit-exp)
    (expression (identifier)
                var-exp)
    
    (expression (primitive "(" (separated-list expression ",") ")" )  
                primapp-exp)
    
    (expression (primitive-one "(" expression ")") primapp-one-exp)
    
    (expression (primitive-two "(" expression "," expression ")") primapp-two-exp)
    
    ; (3.3) conditional
    (expression ("if" expression "then" expression "else" expression)
                if-exp)
    ; (3.4) local binding
    (expression ("let" (arbno  identifier "=" expression) "in" expression)
                let-exp)
    
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)

    ; (3.1) primitives
    (primitive      ("+")       add-prim)
    (primitive      ("*")       mult-prim)
    (primitive      ("cons")    cons-prim)
    (primitive      ("car")     car-prim)
    (primitive      ("cdr")     cdr-prim)
    (primitive      ("list")    list-prim)
    
    (primitive-one  ("add1")    incr-prim)
    (primitive-one  ("sub1")    decr-prim)
    
    (primitive-two  ("-")       subtract-prim)
    ))

; ------------------------------------------------------------
; define datatypes before defining interpreter

(sllgen:make-define-datatypes scanner-spec grammar)
(define dump-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec grammar)))

; ------------------------------------------------------------
;  environment (using ribcage implementation - see 2.3.4)
; - maps identifiers to values

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
    (syms (list-of symbol?))
    (vec  vector?)              ; can use this for anything.
    (env  environment?))
  )

(define empty-env
  (lambda ()
    (extend-env '(emptylist) '(()) (empty-env-record))))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms (list->vector vals) env)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
        (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((position (rib-find-position sym syms)))
          (if (number? position)
              (vector-ref vals position)
              (apply-env env sym)))))))

(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))

; ------------------------------------------------------------
;  evaluator

; evaluate program
(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program      (body)      (eval-expression body (init-env)) ))))

; evaluate expression
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      ; (3.1) literals, variables, primitive applications
      (lit-exp        (datum)    datum)
      (var-exp        (id)        (apply-env env id))
      (primapp-exp    (prim rands)
                      (let ((args (eval-rands rands env)))
                        (apply-primitive prim args) ))
      (primapp-one-exp (prim rand)
                       (let ((arg (eval-rand rand env)))
                         (apply-primitive-one prim arg)))
      
      (primapp-two-exp (prim rand1 rand2)
                       (apply-primitive-two prim (eval-rand rand1 env) (eval-rand rand2 env)))
      
      ; (3.3) conditional
      (if-exp        (test-exp true-exp false-exp)
        (if (true-value? (eval-expression test-exp env))
          (eval-expression true-exp env)
          (eval-expression false-exp env)))
      ; (3.4) local binding
      (let-exp        (ids rands body)
        (let ((args (eval-rands rands env)))
          (eval-expression body (extend-env ids args env))))
      
      (cond-exp (conditions returns)
                (eval-cond conditions returns env))
      )))

; (3.3) 0=false, anything else is true
; - placeholder for other definitions of true & false
(define true-value?
  (lambda (x) (not (zero? x))))

; (3.4) evaluate operands for a procedure call
(define eval-rands
  (lambda (rands env) (map (lambda (x) (eval-rand x env)) rands) ))
(define eval-rand
  (lambda (rand env) (eval-expression rand env) ))

(define eval-cond
  (lambda (conditions returns env)
    (if (null? conditions)
        0
        (if (true-value? (eval-expression (car conditions) env))
            (eval-expression (car returns) env)
            (eval-cond (cdr conditions) (cdr returns) env)))))
        

; find list sum
(define list-sum
  (lambda (lst)
    (if
     (equal? (length lst) 0)
     (eopl:error "Trying to add nothing?!?! Absolutely NOT!")
     (if
      (equal? (length lst) 1)
      (car lst)
      (+ (car lst) (list-sum (cdr lst)))))))

; find list product
(define list-prod
  (lambda (lst)
    (if
     (equal? (length lst) 0)
     (eopl:error "Trying to multiply nothing?!?! Absolutely NOT!")
     (if
      (equal? (length lst) 1)
      (car lst)
      (* (car lst) (list-prod (cdr lst)))))))

; (3.1) apply primitive procedure to arguments
(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim      ()  (list-sum args))
      (mult-prim      ()  (* (list-prod args)))
      (car-prim       ()  (car args))
      (cdr-prim       ()  (cdar args))
      (cons-prim      ()  (cons (car args) (cadr args)))
      (list-prim      ()  args))))

(define apply-primitive-one
  (lambda (prim arg)
    (cases primitive-one prim
      (incr-prim     () (+ arg 1))
      (decr-prim     () (- arg 1)))))

(define apply-primitive-two
  (lambda (prim arg1 arg2)
    (cases primitive-two prim
      (subtract-prim  ()  (- arg1 arg2)))))

; initial environment (named constants only, since it can't be changed)
(define init-env
  (lambda () (extend-env '(i v x) '(1 5 10) (empty-env)) ))

; ------------------------------------------------------------
;  interpreter

(define scan
  (sllgen:make-string-scanner scanner-spec 
                              grammar))
(define scan&parse
  (sllgen:make-string-parser  scanner-spec 
                              grammar))
(define read-dump
  (sllgen:make-rep-loop "--> " (lambda (tree) tree)
                        (sllgen:make-stream-parser scanner-spec
                                                  grammar)))

(define scan&parse&eval
  (lambda (s) (eval-program(scan&parse s))) )

(define read-eval-print
  (sllgen:make-rep-loop "--> " eval-program
                        (sllgen:make-stream-parser scanner-spec 
                                                  grammar)))

; ------------------------------------------------------------
; testing - use (scan), (scan&parse), (read-dump), (read-eval-print) 

(define test-3.1a "add1(2)")

; ignores extra parameters
(define test-3.1b "add1(2,3)")
(define test-3.1c "+(3,4,5)")

; more tests
(define test-3.1d "+(add1(2),-(6,4))")
(define test-3.1e "*(2,+(3,sub1(4)))")
(define test-3.3a "if -(3,+(1,2)) then 2 else 3")
(define test-3.4a "let x = 5 y = 6 in +(x,y)")
(scan&parse test-3.3a)
(scan&parse test-3.4a)

(read-eval-print)