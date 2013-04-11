#lang scheme

;; PROGRAM NAME: hw02.rkt
;; DESCRIPTION: Homework 2 for CSI 310
;; AUTHOR: Ben Burwell
;; HISTORY: Turned in 2013-01-29
;; NOTES: When this program runs, a series of expected and
;;        actual results will print out. Compare them to
;;        ensure the code functions properly.

;; ============ PROCEDURE ============
;; NAME: invert
;; 
;; DESC:
;; takes a list of 2-lists and inverts each of the lists
;; hence, the list
;;     ((2 1) (4 3) (6 5))
;; will become
;;     ((1 2) (3 4) (5 6))

(define invert
  (λ (lst)
    (cond
      [ (null? lst) null ]
      [ (not (list? lst)) (display "Invalid Parameters") ]
      [ (null? (cdr lst)) (list (invert-reverse (car lst))) ]
      [ else (append (list (invert-reverse (car lst))) (invert (cdr lst))) ] )))

;; a helper function that reverses a 2-list
;; PRECONDITION: lst is a list of the form (a b)
;; POSTCONDITION: lst is a list of the form (b a)
;; ERROR: lst is not a list with 2 elements
(define invert-reverse
  (λ (lst)
    (cond
      [ (null? lst) null ]
      [ (not (list? lst)) (display "Invalid Parameters") ]
      [ (not (equal? (length lst) 2)) (display "Invalid Parameters") ]
      [ else (list (cadr lst) (car lst)) ] )))

;; ============ TEST CODE ============
(newline)
(display "Testing (invert) =========================================")
(newline)

(display "Expected output: ()                  ")
(invert '())

(display "Expected output: ((1 2))             ")
(invert '((2 1)))

(display "Expected output: ((1 2) (3 4) (5 6)) ")
(invert '((2 1) (4 3) (6 5)))


;; ============ PROCEDURE ============
;; NAME: vector-index
;; 
;; DESC:
;; returns the zero-based index of the first 
;; occurence of a parameter in a vector, or 
;; -1 if there is no occurrence.
(define vector-index
  (λ (needle haystack)
    (cond
      [ (null? needle) (display "Invalid Parameters") ]
      [ (vector? haystack) (list-index needle (vector->list haystack)) ]
      [ else (display "Invalid Parameters") ] )))

;; a helper function
;; PRECONDITION: haystack is a non-nested list (e.g. the result of vector->list)
(define list-index
  (λ (needle haystack)
    (if (null? haystack)
        -1
        (if (equal? (car haystack) needle)
            0
            (if (equal? (list-index needle (cdr haystack)) -1)
                -1
                (+ 1 (list-index needle (cdr haystack))))))))

;; ============ TEST CODE ============
(newline)
(display "Testing (vector-index) ===================================")
(newline)

(display "Expected output: 2                   ")
(vector-index 3 #(1 2 3 4 5 6 7 8 9))

(display "Expected output: -1                  ")
(vector-index 4 #(1 2 3))

(display "Expected output: -1                  ")
(vector-index 3 #())


;; ============ PROCEDURE ============
;; NAME: count-occurrences
;;
;; DESC:
;; counts the occurrences of needle in haystack
(define count-occurrences
  (λ (needle haystack)
    (cond
      [ (null? needle) (display "Error: nothing to search for") ]
      [ (null? haystack) 0 ]
      [ (list? (car haystack)) (+ (count-occurrences needle (car haystack)) (count-occurrences needle (cdr haystack))) ]
      [ (equal? (car haystack) needle) (+ 1 (count-occurrences needle (cdr haystack))) ]
      [ else (count-occurrences needle (cdr haystack)) ] )))

;; ============ TEST CODE ============
(newline)
(display "Testing (count-occurrences) ==============================")
(newline)

(display "Expected output: 10                  ")
(count-occurrences 'a '(a b a c d (((((((((a))))))))) e f a b a a (d e) (a) c (a (a (a)))))

(display "Expected output: 0                   ")
(count-occurrences 'a '())

(display "Expected output: 1                   ")
(count-occurrences 'a '(a))

;; ============ PROCEDURE ============
;; NAME: compose
;;
;; DESC:
;; takes 1, 2, or 3 procedures and composes them, as specified by the equation:
;;     (compose f g h) = (compose f (compose g h))
(define compose
  (λ funcs
    (cond
      [ (equal? (length funcs) 1) (car funcs) ]
      [ (equal? (length funcs) 2) (λ (x) ((car funcs) ((cadr funcs) x))) ]
      [ (equal? (length funcs) 3) (λ (x) ((car funcs) ((cadr funcs) ((caddr funcs) x)))) ]
      [ else (display "Invalid parameters") ] )))

;; ============ TEST CODE ============
(newline)
(display "Testing (compose) ========================================")
(newline)

(display "Expected output: 1                   ")
((compose car cdr cdr) '(0 2 1 3))

(display "Expected output: 1                   ")
((compose car) '(1 2 3 4))

(display "Expected output: 1                   ")
((compose car cdr) '(0 1 2 3))

(display "Expected output: 1                   ")
((compose - -) 1)

;; =========== END OF FILE ===========