#!/usr/bin/racket
#lang racket
; 1.2.6
; regular test for primes
;; Go through all up to sqrt n, if it divides n then not prime
(define (square x) (* x x))

(define (divides? a b) (= 0 (remainder b a)))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
    ((divides? test-divisor n) test-divisor)
    (else (find-divisor n (+ 1 test-divisor)))))
(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))
; Fermat's test for primes

(define (expmod base exp m)
  ;; this speedup depends on mod(xy, z) = mod(x, z) * mod(y, z)
  (cond ((= exp 0) 1) ; base^0 = 1, 1 mod n = 1
        ((even? exp)  ; get a logarithmic time to compute. 3^4 mod 5 = mod(mod( 3 ^ 2, 5 ) ^ 2, 5)
         (remainder 
           (square (expmod base (/ exp 2) m))
           m))
        (else
          (remainder 
            (* base (expmod base (- exp 1) m))
            m))))

(define (fermat-test n)
  (define (try-it a) 
    (= (expmod a n n) a))
  (try-it (+ (random (- n 1)) 1)))


(define (fast-prime? n times)
  (cond ((= 0 times) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

;; this is a probalistic test for primality. There are numbers that can fool this test, called Carmichael numbers,
;; but these are _very_ rare.
;; 561 is an example. 


;; Ex 1.21
;; Use the smallest-divisor procedure to find smallest divisor of 199, 1999, 19999
(smallest-divisor 199) ; 199
(smallest-divisor 1999) ; 1999
(smallest-divisor 19999) ; 7!

;; Ex 1.22
;; timing prime

(define (timed-prime-test n)
  ;; runtime is a procedure returning the amount of time system has been running
  (start-prime-test n (current-inexact-monotonic-milliseconds)))
  (define (start-prime-test n start-time)
    (if (prime? n)
      (report-prime n (- (current-inexact-monotonic-milliseconds) start-time))
    false))
  (define (report-prime n elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))

; check the primality of consecutive odd integers in a specified range
; first, convert the ranges to 2n + 1 => divide by 2
(define (search-for-primes start end)
  (define (check-prime curr end)
    (timed-prime-test curr)
    (if (< (+ 2 curr) end) (check-prime (+ 2 curr) end) false))
  (if (= 0 (remainder start 2)) (check-prime (+ 1 start) end) (check-prime start end)))

(search-for-primes 1000 1050) ;; smallest primes > 1000: 1009, 1013, 1019
(search-for-primes 10000 10050) ;; 10007, 10009, 10037
(search-for-primes 100000 100050) ;; 100003, 100019, 100043
(search-for-primes 1000000 1000050) ;; 1000003, 1000033, 1000037
;; looking over the results in primes_calc_scaling.csv
;; the scaling by n is not completely consistent, there is some
;; variation (possibly driven by the specifics of calculating that prime -
;; could consider averaging around it, though the values seemed pretty consistent around
;; it). Regardless, we see some proportionality with n


;; Ex 1.23
;; Redefine smallest-divisor
;; 

;; (define (smallest-divisor n) 
;;   (define (divides? a b) (= 0 (remainder b a)))
;;   (define (next n) 
;;     (if (= 2 n) 3 (+ 2 n)))
;;   (define (find-divisor n test-divisor)
;;     (cond ((> (square test-divisor) n) n)
;;       ((divides? test-divisor n) test-divisor)
;;       (else (find-divisor n (next test-divisor)))))
;;   (find-divisor n 2))

(timed-prime-test 1009)
(timed-prime-test 1013)
(timed-prime-test 1019)
(timed-prime-test 10007)
(timed-prime-test 10009)
(timed-prime-test 10037)
(timed-prime-test 100003)
(timed-prime-test 100019)
(timed-prime-test 100043)
(timed-prime-test 1000003)
(timed-prime-test 1000033)
(timed-prime-test 1000037)

;; apply it to prime? and check timing
