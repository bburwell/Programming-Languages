#lang scheme

;; NAME: hw04_bintree.scm
;; AUTHOR: Ben Burwell
;; DESC: CSI310 - Programming Languages - Homework 4 - Binary Tree
;; HISTORY: Created 2013-02-11

;; ========== PROCEDURE ==========
;; NAME: tree?
;; DESC: takes a parameter and returns true if it is a tree
;;       and false otherwise
(define tree?
  (λ (arg)
    ;; a tree has the following properties:
    (and
     
     ;; it is a list
     (list? arg)
     
     ;; it has three elements
     (equal? (length arg) 3)
     
     ;; its first element is a number
     (number? (car arg))
     
     ;; its second element has one of the following:
     (or
      
      ;; it is an empty list...
      (and (list? (cadr arg)) (equal? (length (cadr arg)) 0))
      
      ;; ...or it is a tree
      (tree? (cadr arg)))
     
     ;; its third element also has one of the above conditions
     (or
      (and (list? (caddr arg)) (equal? (length (caddr arg)) 0))
      (tree? (caddr arg)))
     )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (tree?) ==========================================================================")
(newline)

(display "Expected output: #t  ")
(tree? '(1 () ()))

(display "Expected output: #t  ")
(tree? '(5 (2 () ()) ()))

(display "Expected output: #f  ")
(tree? '())

(display "Expected output: #f  ")
(tree? 1)

(display "Expected output: #t  ")
(tree? '( 14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))





;; ========== PROCEDURE ==========
;; NAME: make-tree
;; DESC: makes a tree from three parameters
(define make-tree
  (λ (val lc rc)
    (cond
      [ (and (tree? lc) (tree? rc)) (list val lc rc) ]
      [ (tree? lc) (list val lc '()) ]
      [ (tree? rc) (list val '() rc) ]
      [ else (list val '() '()) ]
      )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (make-tree) ======================================================================")
(newline)

(display "Expected output: (1 () ())                              ")
(make-tree 1 '() '())

(display "Expected output: (5 (4 (3 (2 (1 () ()) ()) ()) ()) ())  ")
(make-tree 5 (make-tree 4 (make-tree 3 (make-tree 2 (make-tree 1 '() '()) '()) '()) '()) '())


;; ========== PROCEDURE ==========
;; NAME: get-value
;; DESC: returns the value of the root element of the
;;       tree parameter, not-a-tree if the argument is
;;       not a tree
(define get-value
  (λ (tree)
    (if (tree? tree)
        (car tree)
        'not-a-tree
        )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (get-value) ======================================================================")
(newline)

(display "Expected output: 1  ")
(get-value (make-tree 1 '() '()))

(display "Expected output: 5  ")
(get-value (make-tree 5 (make-tree 4 (make-tree 3 (make-tree 2 (make-tree 1 '() '()) '()) '()) '()) '()))

;; ========== PROCEDURE ==========
;; NAME: get-left
;; DESC: returns the left child of the tree parameter
;;       or not-a-tree if the parameter is not a tree
(define get-left
  (λ (tree)
    (if (tree? tree)
        (cadr tree)
        'not-a-tree
        )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (get-left) =======================================================================")
(newline)

(display "Expected output: (1 () ())  ")
(get-left (make-tree 5 (make-tree 1 '() '()) '()))

;; ========== PROCEDURE ==========
;; NAME: get-right
;; DESC: returns the right child of the tree parameter
;;       or not-a-tree if the parameter is not a tree
(define get-right
  (λ (tree)
    (if (tree? tree)
        (caddr tree)
        'not-a-tree
        )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (get-right) ======================================================================")
(newline)

(display "Expected output: (1 () ())  ")
(get-right (make-tree 5 '() (make-tree 1 '() '())))

;; ========== HELP FUNC ==========
;; takes a number to find in a bst as well as a path to append to
(define path-helper
  (λ (n bst pth)
    (cond
      
      ;; we have found the path
      [ (equal? (get-value bst) n) pth ]
      
      ;; the element is not in the tree, return not-found
      [ (and (not (tree? (get-left bst))) (not (tree? (get-right bst)))) 'not-found ]
      
      ;; n < value, we must go left
      [ (< n (get-value bst)) (cons 'left (path-helper n (get-left bst) pth)) ]
      
      ;; n > value, we must go right
      [ (> n (get-value bst)) (cons 'right (path-helper n (get-right bst) pth)) ]
    )))
    
;; ========== PROCEDURE ==========
;; NAME: path
;; DESC: takes a number and a binary search tree in which
;;       to find it and returns a list containing the
;;       appropriate "left"s and "right"s to navigate from
;;       the root of the tree to the element
;;
;;       returns not-found if the needle is not in the
;;       haystack.
(define path
  (λ (n bst)
    (cond
      [ (not (tree? bst)) 'not-a-tree ]
      [ else (path-helper n bst '()) ]
      )))

;; ========== TEST CODE ==========
(newline)
(display "Testing (path) ===========================================================================")
(newline)

(display "Expected output: (right left left)  ")
(path 17 '( 14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))) 