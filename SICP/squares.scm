; Practical work from SICP 1.1

(define (square x) (* x x))
(define (sum-of-squares x y) (+ (square x) (square y)))

; Ex. 1.3
(define (sum-of-largest-squares x y z)
  (cond ((and (<= x y) (<= x z)) (sum-of-squares y z))
        ((and (<= y x) (<= y z)) (sum-of-squares x z))
        ((and (<= z x) (<= z y)) (sum-of-squares x y))))
