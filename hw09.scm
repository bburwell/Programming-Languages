#lang eopl

; Ben Burwell
; Dr. Kussmaul
; Programming Languages
;
; 4.3 INTERPRETER
; - this interpreter supports:
;   - (3.x) expressions, bindings, procedures, recursion
;   - (4.2) type-checking
;   - (4.3) abstraction boundaries

; ------------------------------------------------------------
; grammar specification for scanner & parser

(define scanner-spec
  '((whitespace (whitespace)                                   skip)
    (comment    ("%" (arbno (not #\newline)))                  skip)
    (identifier (letter (arbno (or letter digit "_" "-" "?"))) symbol)
    (number     (digit (arbno digit))                          number) ))

(define parser-spec
  '((program    (expression)        stmt-prog)
    ; (3.1) literal numbers and booleans, identifiers, primitive operations
    (expression (number)             lit-expr)
    (expression ("true")            true-expr)
    (expression ("false")          false-expr)
    (expression (identifier)         var-expr)
    (expression (primitive "(" (separated-list expression ",") ")")
                                 primapp-expr)
    ; (3.3) conditionals
    (expression ("if" expression "then" expression "else" expression)
                if-expr)
    ; (3.4) local bindings
    (expression ("let" (arbno identifier "=" expression) "in" expression)
                let-expr)
    ; (3.5) procedure definitions(w/types) & applications
    (expression ("proc" "(" (separated-list type-exp identifier ",") ")" expression)
                proc-expr)
    (expression ("(" expression (arbno expression) ")")
                app-expr)
    ; (3.6) mutually recursive bindings (w/types)
    (expression ("letrec" (arbno type-exp identifier
                                 "(" (separated-list type-exp identifier ",") ")"
                                 "=" expression) "in" expression)
                letrec-expr)
    ; (4.3) type definitions
    (expression ("lettype" identifier "=" type-exp
                           (arbno type-exp identifier
                                  "(" (separated-list type-exp identifier ",") ")"
                                  "=" expression)
                           "in" expression)
                lettype-expr)

    ; (3.1) primitive operations
    (primitive ("+")      add-prim)
    (primitive ("-")      sub-prim)
    (primitive ("*")     mult-prim)
    (primitive ("add1")  incr-prim)
    (primitive ("sub1")  decr-prim)
    (primitive ("zero?") zero-prim)

    ; (4.2) types
    (type-exp ("int")                                                int-type-exp)
    (type-exp ("bool")                                              bool-type-exp)
    (type-exp (identifier)                                           tid-type-exp)
    (type-exp ("(" (separated-list type-exp "*") "->" type-exp ")") proc-type-exp)
    
    ; (4.3) list stuffs
    (primitive ("cons")      cons-prim)
    (primitive ("car")       car-prim)
    (primitive ("cdr")       cdr-prim)
    (primitive ("list")      list-prim)
    (primitive ("emptylist") empty-list-prim)
    (primitive ("null?")     null-prim)
    
    ))

; ------------------------------------------------------------
; define data types before defining interpreter

(sllgen:make-define-datatypes scanner-spec parser-spec)
(define show-the-datatype
  (lambda () (sllgen:list-define-datatypes scanner-spec parser-spec)) )

; ------------------------------------------------------------
; types (4.2-4.3, pg 134, revised pg 147)

(define-datatype type type?
  ; named atomic type
  (atomic-type (name symbol?))
  ; procedure type (parameter types and result type)
  (  proc-type (arg-types (list-of type?)) (result-type type?))
  ( list-type (item-type type?) )
  )

; primitive types
(define  int-type (atomic-type 'int ))
(define bool-type (atomic-type 'bool))

; type-expressions
(define expand-type-expression
  (lambda (texp tenv)
    (cases type-exp texp
      ; (4.2) primitive expression types
      ( int-type-exp ()    int-type)
      (bool-type-exp ()   bool-type)
      ; (4.3) type definitions
      ( tid-type-exp (id) (find-typedef tenv id))
      ; (4.2) procedure types using
      ; (4.3) type environment
      (proc-type-exp (arg-texps result-texp)
                     (proc-type
                      (expand-type-expressions    arg-texps tenv)
                      (expand-type-expression  result-texp  tenv) )) )))

(define expand-type-expressions
  (lambda (texps tenv)
    (map (lambda (texp) (expand-type-expression texp tenv)) texps)))

; check that two types are equal
(define check-equal-type!
  (lambda (t1 t2 exp)
    (or (equal? t1 t2)
        (eopl:error 'type-of-expression
                    "Types didn't match: ~s != ~s in~%~s"
                    (type-to-external-form t1)
                    (type-to-external-form t2)
                    exp))))

; ------------------------------------------------------------
; type checker (pg 136-137)

(define type-of-program
  (lambda (prog)
    (cases program prog
      (stmt-prog (exp) (type-of-expression exp (empty-tenv))))))

(define type-of-expression
  (lambda (expr tenv)
    (cases expression expr
      ; (4.2) literals - have fixed type
      (lit-expr      (number)   int-type)
      (true-expr     ()        bool-type)
      (false-expr    ()        bool-type)
      ; (4.2) variables (identifiers) - look up in type environment
      (var-expr      (id)      (apply-tenv tenv id))
      ; (4.2) conditionals
      (if-expr       (test-expr then-expr else-expr)
                   (let ((test-type (type-of-expression test-expr tenv))
                         (then-type (type-of-expression then-expr tenv))
                         (else-type (type-of-expression else-expr tenv)) )
                     ;; these tests either succeed or raise an error
                     ; check that test-expr has bool-type
                     (check-equal-type! test-type bool-type test-expr)
                     ; check that then-exp and else-exp have the same type
                     (check-equal-type! then-type else-type      expr)
                     ; return result type (then-expr and else-expr have same type)
                     then-type))
      ; (4.2) procedure definitions
      (proc-expr     (texps ids body)
                     (type-of-proc-expr texps ids body tenv))
      ; (4.2) primitive applications
      (primapp-expr  (prim rands)
                     (type-of-application (type-of-primitive prim)
                                          (types-of-expressions rands tenv)
                                          prim rands expr))
      ; (4.2) procedure applications
      (app-expr      (rator rands)
                     (type-of-application (type-of-expression   rator tenv)
                                          (types-of-expressions rands tenv)
                                          rator rands expr))
      ; (4.2) local bindings
      (let-expr      (ids rands body)
                     (type-of-let-expr ids rands body tenv))
      (letrec-expr   (result-texps proc-names texpss idss bodies body)
                     (type-of-letrec-expr
                      result-texps proc-names texpss idss bodies body tenv))
      ; (4.3) type definitions
      (lettype-expr  (type-name texp result-texps proc-names texpss idss bodies body)
                     (type-of-lettype-expr
                      type-name texp
                      result-texps proc-names texpss idss bodies body tenv))
      )))

; get list of types for list of expressions (typically operands)
(define types-of-expressions
  (lambda (rands tenv)
    (map (lambda (expr) (type-of-expression expr tenv)) rands) ))

(define type-of-primitive
  (lambda (prim)
    (cases primitive prim
      ( add-prim () (proc-type (list int-type int-type) int-type))
      ( sub-prim () (proc-type (list int-type int-type) int-type))
      (mult-prim () (proc-type (list int-type int-type) int-type))
      (incr-prim () (proc-type (list int-type)          int-type))
      (decr-prim () (proc-type (list int-type)          int-type))
      (zero-prim () (proc-type (list int-type)         bool-type))
      
      (cons-prim       () 1)
      (car-prim        () 1)
      (cdr-prim        () 1)
      (list-prim       () 1)
      (empty-list-prim () 1)
      (null-prim       () 1)
      )))

(define type-of-proc-expr
  (lambda (texps ids body tenv)
    ; get argument types and result type (must be inferred from procedure body)
    (let ((arg-types (expand-type-expressions texps tenv)))
      (let ((result-type (type-of-expression body (extend-tenv ids arg-types tenv))))
        ; proc type is determined by argument types and result type
        (proc-type arg-types result-type) ))))

(define type-of-application
  (lambda (rator-type rand-types rator rands exp)
    (cases type rator-type
      (proc-type (arg-types result-type)
                 ; if same # of args and rands, and types line up, then return result-type
                 (if (= (length arg-types) (length rand-types))
                     (begin
                       (for-each check-equal-type! rand-types arg-types rands)
                       result-type)
                     (eopl:error 'type-of-expression
                                 (string-append
                                  "Wrong number of arguments in expression"
                                  "~s:~%expected ~s~%got ~s")
                                 exp
                                 (map type-to-external-form  arg-types)
                                 (map type-to-external-form rand-types) )))
      (else (eopl:error 'type-of-expression
                        "Rator not a proc type:~%~s~%had rator type ~s"
                        rator (type-to-external-form rator-type) )) )))

; (4.2)
(define type-of-let-expr
  (lambda (ids rands body tenv)
    (let ((tenv-for-body (extend-tenv ids (types-of-expressions rands tenv) tenv)))
      (type-of-expression body tenv-for-body))))

; (4.2)
(define type-of-letrec-expr
  ; takes set of procedures (result types, names, arg types, arg names, and bodies)
  ;   body in which new type is used, and parent type environment
  (lambda (result-texps proc-names texpss idss bodies letrec-body tenv)
    ; determine arg types, result types, and resulting procedure types
    (let ((arg-typess       ; list of lists of argument types
           (map (lambda (texps) (expand-type-expressions texps tenv)) texpss))
          (result-types     ; list of result types
           (expand-type-expressions result-texps tenv) ))
      (let ((the-proc-types ; list of procedure types
             (map proc-type arg-typess result-types)))
        (let ((tenv-for-body            ; type env for all proc-bodies
               (extend-tenv proc-names the-proc-types tenv)))
          ; check that each procedure body matches declared procedure type
          (for-each
           (lambda (ids arg-types body result-type)
             (check-equal-type!
              (type-of-expression body (extend-tenv ids arg-types tenv-for-body))
              result-type
              body))
           idss arg-typess bodies result-types)
          ; return type of letrec-body
          (type-of-expression letrec-body tenv-for-body))))))

; (4.3)
(define type-of-lettype-expr
  ; takes name of new type, type expression for representation,
  ;   set of procedures (result types, names, arg types, arg names, and bodies)
  ;   body in which new type is used, and parent type environment
  (lambda (type-name texp
                     result-texps proc-names arg-texpss idss bodies
                     lettype-body tenv)
    (let ((the-new-type (fresh-type type-name))
          (rhs-texps    (map proc-type-exp arg-texpss result-texps)))
      (let (
            ; for implementation, defined type is bound to definition (TRANSPARENT)
            (tenv-for-implementation
             (extend-tenv-with-typedef-exp  type-name texp tenv))
            ; for  client (body), defined type is bound to atomic type (OPAQUE)
            (tenv-for-client
             (extend-tenv-with-typedef       type-name the-new-type tenv)))
        (let ((tenv-for-proc                ; type env for all proc-bodies
               (extend-tenv-with-type-exps
                proc-names rhs-texps tenv-for-implementation))
              (tenv-for-body                ; type env for body
               (extend-tenv-with-type-exps
                proc-names rhs-texps tenv-for-client)))
          ; check that each procedure body matches declared procedure type
          (for-each
           (lambda (ids arg-texps body result-texp)
             (check-equal-type!
              (type-of-expression
               body
               (extend-tenv-with-type-exps ids arg-texps tenv-for-proc))
              (expand-type-expression result-texp tenv-for-proc)
              body))
           idss arg-texpss bodies result-texps)
          ; return type of lettype-body
          (type-of-expression lettype-body tenv-for-body))))))

; create fresh type based on parameter name (4.3, pg 149)
; - each time this is called, counter is incremented
(define fresh-type
  (let ((counter 0))
    (lambda (s)
      (set! counter (+ counter 1))
      (atomic-type (string->symbol
                    (string-append (symbol->string s) (number->string counter)) )))))

; extend type environment (4.3, pg 149)
(define extend-tenv-with-typedef-exp
  (lambda (typename texp  tenv)
    (extend-tenv-with-typedef typename (expand-type-expression  texp  tenv) tenv)))

; extend type environment (4.3, pg 149)
(define extend-tenv-with-type-exps
  (lambda (ids      texps tenv)
    (extend-tenv              ids      (expand-type-expressions texps tenv) tenv)))

; ------------------------------------------------------------
; type environments

(define-datatype type-environment type-environment?
  ; (4.2) empty & extended type environments
  (empty-tenv-record)
  (extended-tenv-record (syms       (list-of symbol?))
                        (vals       (list-of type?))
                        (tenv       type-environment?))
  ; (4.3, pg 146) environment extended with new type definition
  (typedef-record       (name       symbol?)
                        (definition type?)
                        (tenv       type-environment?) ))

(define  empty-tenv                 empty-tenv-record)
(define extend-tenv              extended-tenv-record)
(define extend-tenv-with-typedef       typedef-record)

; (4.3)
(define apply-tenv
  (lambda (tenv sym)
    (cases type-environment tenv
      (empty-tenv-record    ()
                            (eopl:error 'apply-tenv
                                        "Variable ~s unbound in type environment" sym))
      (extended-tenv-record (syms vals tenv)
                            (let ((pos (list-find-position sym syms)))
                              (if (number? pos)
                                  (list-ref vals pos) (apply-tenv tenv sym))))
      (typedef-record       (name type tenv)
                            (apply-tenv tenv sym)))))

; (4.3)
(define find-typedef
  (lambda (tenv0 sym)
    (let loop ((tenv tenv0))
      (cases type-environment tenv
        (empty-tenv-record ()
                           (eopl:error 'apply-tenv
                                       "Type variable ~s unbound in type environment ~s"
                                       sym tenv0))
        (extended-tenv-record (syms vals tenv) (loop tenv))
        (typedef-record (name type tenv)
                        (if (eqv? name sym) type (loop tenv)))))))

; ------------------------------------------------------------
; external form of types (pg 135)

(define type-to-external-form
  (lambda (ty)
    (cases type ty
      (atomic-type (name) name)
      (proc-type (arg-types result-type)
                   (append (arg-types-to-external-form arg-types) '(->)
                           (list (type-to-external-form result-type)) ))
      (list-type (item-type) 'asdf)
                  )))

(define arg-types-to-external-form
  (lambda (types)
    (if (null? types)
        '()
        (if (null? (cdr types))
            (list (type-to-external-form (car types)))
            (cons (type-to-external-form (car types))
                  (cons '* (arg-types-to-external-form (cdr types)) ))))))

; ------------------------------------------------------------
; interpreter

(define eval-expression
  (lambda (expr env)
    (cases expression expr
      (lit-expr      (datum) datum)
      (true-expr     () 1)
      (false-expr    () 0)
      (var-expr      (id) (apply-env env id))
      (primapp-expr  (prim rands)
                     (let ((args (eval-primapp-expr-rands rands env)))
                       (apply-primitive prim args)))
      (if-expr       (test-expr then-expr else-expr)
                     (if (true-value? (eval-expression test-expr env))
                       (eval-expression then-expr env)
                       (eval-expression else-expr env)))
      (let-expr      (ids rands body)
                     (let ((args (eval-rands rands env)))
                       (eval-expression body (extend-env ids args env))))
      (proc-expr     (texps ids body)
                     (closure ids body env))
      (app-expr      (rator rands)
                     (let ((proc (eval-expression  rator env))
                           (args (eval-rands       rands env)))
                       (if (procval? proc)           ; always true in typechecked code
                           (apply-procval proc args)
                           (eopl:error 'eval-expr "Attempt to apply non-proc ~s" proc) )))
      (letrec-expr   (result-texps proc-names texpss idss bodies letrec-body)
                     (eval-expression letrec-body
                                      (extend-env-recursively proc-names idss bodies env)))
      (lettype-expr  (type-name texp
                                result-texps proc-names texpss
                                idss bodies lettype-body)
                     (eval-expression lettype-body
                                      (extend-env-recursively proc-names idss bodies env)))

      )))

(define eval-program
  (lambda (prog)
    (cases program prog
      (stmt-prog (body) (eval-expression body (empty-env))) )))

(define eval-primapp-expr-rands
  (lambda (rands env)
    (map (lambda (x) (eval-expression x env)) rands)))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      ( add-prim () (+ (car args) (cadr args)))
      ( sub-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (zero-prim () (if (zero? (car args)) 1 0))
      
      (cons-prim       () (cons (car args) (cadr args)))
      (car-prim        () (car args))
      (cdr-prim        () (cdr args))
      (list-prim       () args)
      (empty-list-prim () '())
      (null-prim       () ((if (null? (args)) 1 0)))
      )))

; ------------------------------------------------------------
; booleans

(define true-value? (lambda (x) (not (zero? x))))

; ------------------------------------------------------------
; procedures

(define-datatype procval procval?
  (closure
   (ids  (list-of symbol?))
   (body expression?)
   (env  environment?) ))

(define apply-procval
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env ids args env))))))

; ------------------------------------------------------------
; environments

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (syms (list-of symbol?))
   (vals vector?)
   (env  environment?)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record    ()
                           (eopl:error 'apply--env "No binding for ~s" sym))
      (extended-env-record (syms vals old-env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (vector-ref vals pos)
                                 (apply-env old-env sym)))))))

(define empty-env
  (lambda ()              (empty-env-record)))
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
           (iota len) idss bodies)
          env)))))

(define rib-find-position
  (lambda (sym los) (list-find-position sym los)))

(define list-find-position
  (lambda (sym los) (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                  (+ list-index-r 1)
                  #f))))))

(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '() (cons next (loop (+ 1 next)))) )))

; ------------------------------------------------------------
; interpreter

(define scan       (sllgen:make-string-scanner scanner-spec parser-spec))
(define scan&parse (sllgen:make-string-parser  scanner-spec parser-spec))


(define ext-type-of-program
  (lambda (program)
    (type-to-external-form (type-of-program program)) ))
(define ext-type-and-eval-program
  (lambda (program)
    (display "type= ") (display (ext-type-of-program program)) (newline)
    (display (eval-program program)) (newline) ))

; string versions
(define internal-type
  (lambda (string) (          type-of-program (scan&parse string))) )
(define external-type
  (lambda (string) (      ext-type-of-program (scan&parse string))) )
(define run
  (lambda (string) (             eval-program (scan&parse string))) )
(define type&run
  (lambda (string) (ext-type-and-eval-program (scan&parse string))) )

; loop versions
(define stream-parser (sllgen:make-stream-parser scanner-spec parser-spec))
; prints internal type or external type for each entered expression
(define read-internal-type
  (sllgen:make-rep-loop "--> "           type-of-program stream-parser))
(define read-external-type
  (sllgen:make-rep-loop "--> "       ext-type-of-program stream-parser))
; eval each entered expression without or with type checking
(define read-eval-print
  (sllgen:make-rep-loop "--> "              eval-program stream-parser))
(define read-type-eval-print
  (sllgen:make-rep-loop "--> " ext-type-and-eval-program stream-parser))

; ------------------------------------------------------------
; tests

(define test-4a "+(3,4)")
(define test-4b "if zero?(-(2,3)) then 1 else *(2,4)")

; proc that takes int and adds 1
(define test-4.2a  "proc (int x) add1(x)")
(define test-4.2b "(proc (int x) add1(x) 12)")
; recursive proc that takes int and returns factorial
(define test-4.2c "letrec int fact (int x)
                                = if zero?(x) then 1 else *(x,(fact sub1(x)))
                   in fact")
(define test-4.2d "letrec int fact (int x)
                                = if zero?(x) then 1 else *(x,(fact sub1(x)))
                   in (fact 4)")

(type&run test-4.2a)
(type&run test-4.2b)
(type&run test-4.2c)
(type&run test-4.2d)

; type "myint" which represents 0 as 1, etc.
(define test-4.3a "lettype myint = int
    myint    zero()        = 1
    myint    succ(myint x) = add1(x)
    myint    pred(myint x) = sub1(x)
    bool  iszero?(myint x) = zero?(sub1(x))
  in (iszero?(pred(succ(zero))))")
(type&run test-4.3a)

; type "ff" for finite functions (map ints to ints)
(define test-4.3b "lettype ff = (int -> int)
    ff   zero-ff()                          % create ff that returns 0 (base case)
      = proc (int k) 0
    ff extend-ff(int k, int val, ff old-ff) % extend ff with new pair
      = proc (int k1)
          if zero?(-(k1,k)) then val else (apply-ff old-ff k1)
    int apply-ff(ff f, int k) = (f k)       % apply ff to value
  in let ff1 = (extend-ff 1 11 (extend-ff 2 22 (zero-ff)))
    in (apply-ff ff1 2)")
(type&run test-4.3b)