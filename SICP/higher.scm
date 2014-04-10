; general pattern of iteration
;  operation: a function with which to combine terms, e.g. + for summation
;  zero: the 'base' term, e.g. 0 for summation
;  term: a function which gives one term of the iteration
;  a, b: the range of the iteration
;  next: the 'step' function which given one number in (a,b) produces the next
(define (iterator operation zero term a next b)
  (if (> a b)
      zero
      (operation (term a) (iterator operation zero term (next a) next b))))

; sum and product defined as special cases of iteration
(define (SIGMA term a next b) (iterator + 0 term a next b))
(define (PI term a next b) (iterator * 1 term a next b))

(define (inc a) (+ a 1))
(define (cube a) (* a a a))
(define (sum-cubes a b) (SIGMA cube a inc b))

(define (identity a) a)
(define (sum-integers a b) (SIGMA identity a inc b))

(define (pi-sum a b)
  (define (pi-term x) (/ 1.0 (* x (+ x 2))))
  (define (pi-next x) (+ x 4))
  (SIGMA pi-term a pi-next b))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (SIGMA f (+ a (/ dx 2.0)) add-dx b) dx))
