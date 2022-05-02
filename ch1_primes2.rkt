#!/usr/bin/racket
#lang racket

; Testing for primes
;
; 1: Naive method to check n prime
; for 2 up to sqrt (n) none must divide n
; Approach 1a: Naive
; Approach 1b: Naive, with only looking at odds (skip half the space)
;   (so, we expect it to take half the time)
; Approach 2: Fermat's little theorem
;  Pick a random a < n, if a ^ n mod n === a (mod n)
;  That makes it more likely that n is prime! In other words,
;  if it is not true, then likely n is not prime. Not guaranteed.
;
;  Lab notes:
;     wrote ch1_primes.rkt, with the above 3 methods. Found that
;     the performance of `search-for-primes` is... somehow different than
;     when running the prime checks on their own? Very weird
;     But consistent - basically an order of magnitude faster to run `search-for-primes`
;     ... is there something about how we're measuring time?
;     ... is it that we are running similar ops repeatedly?
;     ... is it that we somehow have a strange state (
;       the version of the function being run is not what we
;       expect, since the exercises have us adjust the 
;       code over time?)

; ========================================== ;
;         **** Approach 1a ****
(define (naive-timed-prime-test n)
    (define (prime? n)
        (define (square x) (* x x))
        (define (divides? a b) (= 0 (remainder b a)))
        (define (find-divisor n test-divisor)
          (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ 1 test-divisor)))))
        (define (smallest-divisor n) (find-divisor n 2))
        (= (smallest-divisor n) n))
    (define (start-prime-test n start-time)
        (prime? n)
        (- (current-inexact-monotonic-milliseconds) start-time))
    (start-prime-test n (current-inexact-monotonic-milliseconds)))

(naive-timed-prime-test 1009)

;; the main issue with this average function is that we would like
;; to be able to apply f to arguments, but we dont have a way to do that?
;; hmm... is there a way? we could do a single argument... that will be sufficient
;; for now.
(define (average fn arg n)
  (define (average-iter f arg i total end)
    (if (= n i) (/ total end) (average-iter f arg (+ 1 i) (+ (f arg) total) end)))
  (average-iter fn arg 0 0 n))

(define (square x) (* x x))

; Verify it works with a dumb example!
(average square 2 10)

; ========================================== ;
;         **** Approach 1b ****

; Approach 2: Fermat's little theorem
; 
