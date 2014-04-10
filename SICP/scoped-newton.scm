; Practical work from SICP 1.1

; Ex. 1.7 using block scoping
(define (scoped-sqrt x)
  (define (sqrt-iter newguess oldguess)
    (if (good-enough? newguess oldguess)
       newguess
       (sqrt-iter (improve newguess) newguess)))

  (define (good-enough? newguess oldguess)
    (< (/ (abs (- newguess oldguess)) newguess) 0.0000001)) 

  (define (improve guess) (average guess (/ x guess)))

  (define (average x y) (/ (+ x y) 2))

  (sqrt-iter 1.0 0.5))

; Ex. 1.8 using block scoping
(define (scoped-3root x)
  (define (cube-iter newguess oldguess)
    (if (good-enough? newguess oldguess)
       newguess
       (cube-iter (improve newguess) newguess)))

  (define (good-enough? newguess oldguess)
    (< (/ (abs (- newguess oldguess)) newguess) 0.0000001))

  (define (improve guess) (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

  (cube-iter 1.0 0.5))
