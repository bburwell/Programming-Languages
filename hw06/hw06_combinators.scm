#lang eopl

; Ben Burwell
; Dr. Kussmaul
; CSI-310 Programming Languages
; HW06 :: Combinators

; A. evaluate boolean logic expression
(or F (and T (not F)))
(or F (and T T))
(or F T)
T

; B. compute 2+1 using Church numerals
(increment 2)

((λ (n) (λ (f) (λ (x) (f ((n                         f) x))))) (λ (f) (λ (x) (f (f x)))))
(        λ (f) (λ (x) (f (((λ (f) (λ (x) (f (f x)))) f) x))))
(        λ (f) (λ (x) (f (        (λ (x) (f (f x)))     x))))
(        λ (f) (λ (x) (f                 (f (f x)))))
(λ (f) (λ (x) (f (f (f x)))))