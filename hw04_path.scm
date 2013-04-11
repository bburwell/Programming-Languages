#lang scheme

;; NAME: hw04_bintree.scm
;; AUTHOR: Ben Burwell
;; DESC: CSI310 - Programming Languages - Homework 4 - Binary Tree
;; HISTORY: Created 2013-02-11

(define path-helper
  (λ (n bst pth)
    [ (equal? (get-value bst) n) pth ]
    [ (< n (get-value bst)) (cons 'left (path-helper n (get-left bst))) ]
    [ (> n (get-value bst)) (cons 'right (path-helper n (get-right bst))) ]
    ))
    

(define path
  (λ (n bst)
    (cond
      [ (not (tree? bst)) 'not-a-tree ]
      [ else (path-helper n bst '()) ]
      )))