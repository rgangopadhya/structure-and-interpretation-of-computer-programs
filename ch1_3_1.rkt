#!/usr/bin/racket
#lang racket

(define (cube x)
  (* x x x))

(define (alt-sum-integers a b)
  (if (> a b)
    0
    (+ a (sum-integers (+ a 1) b))))

(define (alt-sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (+ a 1) b))))

(define (alt-pi-sum a b)
  (if (> a b)
    0
    (+ (/ 1.0 (* a (+ a 2)))
       (pi-sum (+ a 4) b))))


(display (alt-pi-sum 1 10))
;; these procedures share an underlying pattern

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (inc n)
  (+ n 1))

(define (sum-cubes a b)
  (sum cube a inc b))

(display (inc 2))
(sum-cubes 1 10)

(define (identity x) x)
(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(display (* 8 (pi-sum 1 1000)))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(display (integral cube 0 1 0.01))
(display (integral cube 0 1 0.001))

;; Ex 1.29
;; Simpson's Rule: more accurate integration
;; method. Integral of a function f between a and b.
(define (simpsons-integral f a b n)
  (define h (/ (- b a) (* 1.0 n)))
  (define (y k) (f (+ a (* k h))))
  (define (factor k)
    (cond ((= (modulo k 2) 0) 2)
          ((= (modulo k 2) 1) 4)
          (else 1)))
  (define (termy k) (* (factor k) (y k)))
  (* (/ h 3.0) (sum termy 0 inc n)))

(display (simpsons-integral cube 0 1 100))
(display (simpsons-integral cube 0 1 1000))
(display (simpsons-integral cube 0 1 10000))

;; 1.30
(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (+ (term a) result))))
  (iter a 0))

(sum-iter identity 0 inc 10)

;; 1.31

(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(display (product identity 1 inc 10))

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (* (term a) result))))
  (iter a 1))

(display (product-iter identity 1 inc 10))

(define (factorial n)
  (if (= n 0)
    1
    (product identity 1 inc n)))

(display (factorial 5))

; 2 4 4 6 6 8
; 0 1 2 3 4 5
; (ceil(n / 2) + 1) * 2
; 3 3 5 5 7 7
; 0 1 2 3 4 5
; ceil((n + 1) / 2) * 2 + 1
(define (pi-wallis n)
  (define (numer k)
    (* 2.0
       (+ 1
          (ceiling (/ k 2)))))
  (define (denom k)
    (+ 1
       (* 2.0
          (ceiling (/ (+ k 1) 2)))))
  (define (termy k) (/ (numer k) (denom k)))
  (*
    4
    (product-iter termy 0 inc n))
  )

(displayln (pi-wallis 5))
(displayln (pi-wallis 100))
(displayln (pi-wallis 1000))
(displayln (pi-wallis 10000))
(displayln (pi-wallis 100000))
(displayln (pi-wallis 10000000))

;; 1.32
;; a. Show that sum and product are both special cases of a still more general notion called accumulate:
;; (accumulate combiner null-value term a next b)
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define (sum-acc term a next b)
  (accumulate + 0 term a next b))

(display (sum-acc identity 0 inc 10))

(define (prod-acc term a next b)
  (accumulate * 1 term a next b))

(display (prod-acc identity 1 inc 10))

(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a) (accumulate-recur combiner null-value term (next a) next b))))
