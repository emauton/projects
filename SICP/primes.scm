(require mzlib/cml)

(define (divides? a b) (= (remainder b a) 0))
(define (square a) (* a a))

(define (smallest-divisor x)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor x 2))

(define (prime-by-division? n) (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                     m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                     m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (exhaustive-fermat-test n)
  (define (iter count)
    (cond ((= count n) #t)
          ((= (expmod count n n) count) (iter (+ count 1)))
          (else #f)))
  (iter 2))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (timed-prime-test n)
  (display n)
  (start-prime-test n (current-time))
  (newline))

(define (start-prime-test n start-time)
  (if (fermat-test n)
    (report-prime (- (current-time) start-time))
    (display "")))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes lower upper)
  (define (iter current)
    (cond ((>= current upper) (display ""))
          ((divides? 2 current) (iter (+ current 1)))
          (else (timed-prime-test current) (iter (+ current 2)))))
  (iter lower))

(define (next n)
  (if (= n 2) 3
      (+ n 2)))

(define (smallest-divisor-faster x)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (find-divisor x 2))

(define (prime-by-division-faster? n) (= n (smallest-divisor-faster n)))
