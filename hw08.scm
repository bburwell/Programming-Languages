#lang eopl

;; Ben Burwell
;; Homework 8
;; Programming Languages
;; Dr. Kussmaul
;; Due: 15 April 2013

;3.9 INTERPRETER
; - the interpreter consists of several components
;   - SCANNER     - converts characters to tokens
;   - PARSER      - converts tokens to abstract syntax trees
;   - ENVIRONMENT - maps identifiers to values
;   - EVALUATOR   - converts abstract syntax trees to values/results
; - this interpreter supports:
;   - (3.1) basic functionality
;     - whitespace & comments (skipped)
;     - integers & built-in defined constants
;     - 3 binary primitive operations - add, sub, mult
;     - 2  unary primitive operations - incr, decr
;   - (3.3) conditionals        - if-then-else
;   - (3.4) local bindings      - let
;   - (3.5) procedures          - proc, app-expr
;   - (3.6) recursion           - letrec
;   - (3.7) variable assignment - set
;   - (3.9) statements          - assign, print, compound, if, while, block
;
; THINGS DO DO (save as 3.9a)

; ------------------------------------------------------------
; scanner specification

(define scanner-spec
  '(
    (whitespace (whitespace)                            skip)
    (comment    ("%" (arbno (not #\newline)))           skip)
    (identifier (letter (arbno (or letter digit "?")))  make-symbol)
    (number     (digit (arbno digit))                   make-number) ))

; ------------------------------------------------------------
; grammar specification

(define grammar
  '(
    ; (3.1) program
    (program (statement) a-program)
    
    ; (3.9) statements
    (statement (identifier "=" expression)
               assign-stmt)
    (statement ("print" "(" expression ")")
               print-stmt)
    (statement ("{" (separated-list statement ";") "}")
               compound-stmt)
    (statement ("if" expression statement statement)
               if-stmt)
    (statement ("while" expression "do" statement)
               while-stmt)
    (statement ("var" (separated-list identifier ",") ";" statement)
               block-stmt)
    
    ; (3.1) expressions
    (expression (number)     lit-expr)
    (expression (identifier) var-expr)
    (expression (primitive "(" (separated-list expression ",") ")")
                primapp-expr)
    
    ; (3.3) conditional evaluation
    (expression ("if" expression "then" expression "else" expression)
                if-expr)
    ; (3.4) local binding
    (expression ("let" (arbno  identifier "=" expression) "in" expression)
                let-expr)
    
    ; (3.5) procedure definition & application
    (expression ("proc" "(" (separated-list identifier ",") ")" expression)
                proc-expr)
    (expression ("(" expression (arbno expression) ")")
                app-expr)
    
    ; (3.6) recursion
    (expression ("letrec"
                 (arbno identifier "(" (separated-list identifier ",") ")"
                        "=" expression)
                 "in" expression)
                letrec-expr)
    
    ; (3.7) variable assignment
    (expression ("set" identifier "=" expression)
                varassign-expr)
    
    ; (3.7 HW) sequential expressions
    (expression ("begin" expression (arbno ";" expression) "end")
                begin-expr)
    
    ; (3.1) primitives
    (primitive ("+")     add-prim)
    (primitive ("-")     sub-prim)
    (primitive ("*")    mult-prim)
    (primitive ("add1") incr-prim)
    (primitive ("sub1") decr-prim)
    ; (3.3) primitives
    (primitive ("equal?")      equal-prim)
    (primitive ("zero?")        zero-prim)
    (primitive ("greater?")  greater-prim)
    (primitive ("less?")        less-prim)
    ))

; ------------------------------------------------------------
; define datatypes before defining interpreter

(sllgen:make-define-datatypes scanner-spec grammar)
(define dump-datatypes
  (lambda () (sllgen:list-define-datatypes scanner-spec grammar)))

; ------------------------------------------------------------
; booleans

(define truth   1)
(define falsity 0)

(define true-value? (lambda (x) (not (zero? x))))

; ------------------------------------------------------------
; environments

(define init-env
  (lambda () (extend-env '(stdout) '(*uninitialized*) (empty-env)) ))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vec vector?)              ; can use this for anything
   (env environment?) ))

(define empty-env (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env) (extended-env-record syms (list->vector vals) env)))

(define extend-env-recursively
  (lambda (proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record proc-names vec old-env)))
          (for-each
           (lambda (pos ids body)
             (vector-set! vec pos (closure ids body env)))
           (iota len)
           idss
           bodies)
          env)))))

(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env "no association for symbol ~s" sym))
      (extended-env-record (syms vals env)
                           (let ((position (env-find-position sym syms)))
                             (if (number? position)
                                 (a-ref position vals) (apply-env-ref env sym) ))))))

(define apply-env
  (lambda (env sym) (deref (apply-env-ref env sym))))

(define env-find-position
  (lambda (sym los) (list-find-position sym los)))

(define list-find-position
  (lambda (sym los) (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r) (+ list-index-r 1) #f))))))

; returns list of integers from 0 to (n-1)
(define iota
  (lambda (n) (do ((n n (- n 1)) (x '() (cons (- n 1) x))) ((zero? n) x))))

; ------------------------------------------------------------
; evaluator

; evaluator for root of abstract syntax tree
(define execute-program
  (lambda (pgm)
    (cases program pgm
      (a-program (statement)
                 (execute-statement statement (init-env)) ))))

; (3.9) statement evaluator
(define execute-statement
  (lambda (stmt env)
    (cases statement stmt
      
      (assign-stmt (id exp)
                   (setref!
                    (apply-env-ref env id)
                    (eval-expression exp env) ))
      
      (print-stmt (exp)
                  (write (eval-expression exp env)) (newline) )
      
      (compound-stmt (statements)
                     (for-each
                      (lambda (statement) (execute-statement statement env))
                      statements) )
      
      (if-stmt (exp true-stmt false-stmt)
               (if (true-value? (eval-expression exp env))
                   (execute-statement  true-stmt env)
                   (execute-statement false-stmt env) ))
      
      ; alternate "named" LET syntax
      (while-stmt (exp statement)
                  (let loop ()
                    (if (true-value? (eval-expression exp env))
                        (begin
                          (execute-statement statement env)
                          (loop) )
                        '() )))
      
      (block-stmt (ids statement)
                  (execute-statement statement
                                     (extend-env ids (map (lambda (id) 0) ids) env) ))
      )))

; expression evaluator
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-expr (datum) datum)
      (var-expr (id) (apply-env env id))
      
      (primapp-expr (prim rands)
                    (let ((args (eval-rands rands env)))
                      (apply-primitive prim args)))
      
      ; (3.3) conditional evaluation
      (if-expr (test-expr true-expr false-expr)
               (if (true-value? (eval-expression test-expr env))
                   (eval-expression  true-expr env)
                   (eval-expression false-expr env) ))
      
      ; (3.4) local binding
      (let-expr (ids rands body)
                (let ((args (eval-rands rands env)))
                  (eval-expression body (extend-env ids args env))))
      
      ; (3.5) procedure definition
      (proc-expr (ids body) (closure ids body env))
      
      ; (3.5) procedure application
      (app-expr (rator rands)
                (let ((proc (eval-expression rator env))
                      (args (eval-rands      rands env)) )
                  (if (procval? proc)
                      (apply-procval proc args)
                      (eopl:error 'eval-expression
                                  "attempt to apply non-procedure ~s"
                                  proc))))
      
      ; (3.6) recursion
      (letrec-expr (proc-names idss bodies letrec-body)
                   (eval-expression letrec-body
                                    (extend-env-recursively proc-names idss bodies env)))
      
      ; (3.7) variable assignment
      (varassign-expr (id rhs-expr)
                      (begin (setref!
                              (apply-env-ref env id)
                              (eval-expression rhs-expr env))
                             1))
      
      ; (3.7) sequential expressions
      (begin-expr (exp1 exps)
                  (letrec
                      ((loop (lambda (acc exps)
                               (if (null? exps) acc
                                   (loop (eval-expression (car exps) env)
                                         (cdr exps))))))
                    (loop (eval-expression exp1 env) exps)))
      )))

; (3.4) evaluate operands
(define eval-rands
  (lambda (rands env) (map (lambda (x) (eval-rand x env)) rands)))
(define eval-rand
  (lambda (rand  env) (eval-expression rand env)))

; (3.1) apply primitive operations
(define apply-primitive
  (lambda (prim args)
    (define check
      (lambda (arity)
        (if (not (= arity (length args)))
            (eopl:error 'apply-primitive
                        "wrong number of arguments to primitive ~s"
                        prim)
            '()
            )))
    (cases primitive prim
      (     add-prim () (check 2) (+ (car args) (cadr args)))
      (     sub-prim () (check 2) (- (car args) (cadr args)))
      (    mult-prim () (check 2) (* (car args) (cadr args)))
      (    incr-prim () (check 1) (+ (car args) 1))
      (    decr-prim () (check 1) (- (car args) 1))
      (   equal-prim () (check 2) (if (= (car args) (cadr args)) truth falsity))
      (    zero-prim () (check 1) (if (zero? (car args))         truth falsity))
      ( greater-prim () (check 2) (if (> (car args) (cadr args)) truth falsity))
      (    less-prim () (check 2) (if (< (car args) (cadr args)) truth falsity))
      )))

; ------------------------------------------------------------
; procedures (3.5)

(define-datatype procval procval?
  (closure
   (ids (list-of symbol?)) ; formal parameters
   (body expression?)      ; procedure body
   (env environment?)))    ; closure (calling environment)

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (if (= (length ids) (length args))
                   (eval-expression body (extend-env ids args env))
                   (eopl:error 'apply-procval
                               "wrong number of arguments to procedure ~s"
                               ids))))))

; ------------------------------------------------------------
; references - for variable assignment (3.7)

(define-datatype reference reference?
  (a-ref
   (position integer?)
   (vec       vector?) ))

(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos) ))))

(define primitive-setref!
  (lambda (ref value)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos value)))))

(define  deref  (lambda (ref)       (primitive-deref   ref)))
(define setref! (lambda (ref value) (primitive-setref! ref value)))

; ------------------------------------------------------------
; interpreter

; scanner only, and parser only
(define scan
  (sllgen:make-string-scanner scanner-spec grammar))
(define scan&parse
  (sllgen:make-string-parser  scanner-spec grammar))
(define read-dump
  (sllgen:make-rep-loop "--> " (lambda (tree) tree)
                        (sllgen:make-stream-parser scanner-spec grammar)))

(define scan&parse&eval
  (lambda (s) (execute-program (scan&parse s))) )

(define read-eval-print
  (sllgen:make-rep-loop "--> " execute-program
                        (sllgen:make-stream-parser scanner-spec grammar)))

; ------------------------------------------------------------
; tests

(define test-3.1a "add1(2)")
; ignores extra parameters
(define test-3.1b "add1(2,3)")
(define test-3.1c "+(3,4,5)")
; more tests
(define test-3.1d "+(add1(2),-(6,4))")
(define test-3.1e "*(2,+(3,sub1(4)))")
(define test-3.3a "if -(3,+(1,2)) then 2 else 3")
(define test-3.4a "let x = 5 y = 6 in +(x,y)")

(define test3.5a "print(let f=proc(y,z) +(y,-(z,5)) in (f 2 28))")
(define test3.5b "print((proc (x) +(x,2) 4))")
(define test3.5c "print(let x=5 in let f=proc(y,z) +(y,-(z,x)) x=28 in (f 2 x))")

(define test3.6a "print(letrec fact(x)=if zero?(x) then 1 else *(x,(fact sub1(x)))
                         in (fact 6))")
(define test3.6b "print(letrec even(x)=if zero?(x) then 1 else (odd sub1(x))
                                odd(x)=if zero?(x) then 0 else (even sub1(x))
                         in (odd 13))")

(define test3.9a "var x,y; {x=3; y=4; print(+(x,y))}")
(define test3.9b "var x,y,z; {x=3; y=4; z=0;
                    while x do {z=+(z,y); x=sub1(x)}; print(z)}")

(define all-tests (list test3.5a test3.5b test3.6a test3.6b test3.9a test3.9b))

(scan&parse&eval test3.5a)
(scan&parse&eval test3.5b)
(scan&parse&eval test3.5c)
(scan&parse&eval test3.6a)
(scan&parse&eval test3.6b)
;(scan test3.9a)
;(scan test3.9b)
;(scan&parse test3.9a)
;(scan&parse test3.9b)
(scan&parse&eval test3.9a)
(scan&parse&eval test3.9b)