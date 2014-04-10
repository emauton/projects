; Practical work from SICP 1.1

(define (newton-sqrt x) (sqrt-iter 1.0 x))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
     guess
     (sqrt-iter (improve guess x) x)
  )
)

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001)
)

(define (improve guess x)
  (average guess (/ x guess))
)

(define (average x y)
  (/ (+ x  y) 2)
)

; Ex. 1.6
(define (new-if protasis apodosis alternative)
  (cond (protasis apodosis)
        (else alternative)
  )
)

(define (broken-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (broken-sqrt-iter (improve guess x) x)
  )
)

; Ex. 1.7
(define (better-newton-sqrt x) (better-sqrt-iter 1.0 0.5 x))

(define (better-sqrt-iter newguess oldguess x)
  (if (better-good-enough? newguess oldguess)
     newguess
     (better-sqrt-iter (improve newguess x) newguess x)
  )
)

(define (better-good-enough? newguess oldguess)
  (< (/ (abs (- newguess oldguess)) newguess) 0.0000001)
)

; Ex 1.8
(define (cube-root x) (cube-root-iter 1.0 0.5 x))

(define (cube-root-iter newguess oldguess x)
  (if (cube-good-enough? newguess oldguess)
     newguess
     (cube-root-iter (cube-improve newguess x) newguess x)
  )
)

(define (cube-good-enough? newguess oldguess)
  (< (/ (abs (- newguess oldguess)) newguess) 0.0000001)
)

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3)
)
