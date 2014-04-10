(define (basic-expt b n)
  (if (= n 0)
      1
      (* b (basic-expt b (- n 1)))))

(define (iter-expt b n)
  (define (iter counter product)
    (if (= counter 0)
        product
        (iter (- counter 1)
              (* b product))))
  (iter n 1))

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-iter-expt b n)
  (define (iter base exponent state)
    (cond ((= exponent 0) state)
          ((even? exponent) (iter (square base) (/ exponent 2) state))
          (else (iter base (- exponent 1) (* base state)))))
  (iter b n 1))
