#!/usr/bin/racket
#lang racket

;; Half-interval method
;; Given a function f, and an interval a-b,
;; such that f(a) < 0 < f(b), then there must be a
;; value x between and b such that f(x) = 0. This is
;; a root of f(x) = 0. Let x be the average of a and b,
;; if f(x) > 0, then the root is between a and x, otherwise
;; the root is between x and b. Continue doing this until we get
;; close enough. This is a O(log(L/T) process, L length of interval, T
;; error tolerance

(define (average x y)
  (/ (+ x y) 2))

(define (close-enough? guess x)
  (< (abs (- guess x)) 0.001))

(define (search f neg-point pos-point)
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

;; roots of x^3 - 2x - 3 = 0 between 1 and 2
(half-interval-method 
  (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

;; Finding fixed points of functions: a fixed point
;; for some function f is an input x s.t. f(x) = x
;; For some functions, this can be found by starting with
;; a guess x, and repeatedly applying f to it until the value
;; doesn't change much.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (x) (+ (sin x) (cos x))) 1.0)

;; Similar to what we did for finding the square root.
;; Given we want to find the square root of x, we want to find the
;; fixed point for f(y) = x / y.
;; We can't simply use this, because we'll get an infinite loop.
;; So, let's do an average of the guess x / y with y
;; average damping is helpful in getting convergence of fixed-point searches

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (square x) (* x x))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y))) 1.0))

(sqrt 6)

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y)))) 1.0))

(cube-root 6)

;; Newton's method
;; if g(x) is differentiable, then a solution of the equation
;; g(x) = 0 is a fixed point of the function f(x), where f(x) = x - g(x)/g'(x)

(define dx 0.00001)
(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (alt-sqrt x)
  (newtons-method
    (lambda (y) (- (square y) x)) 1.0))

;; exercise 1.40
;; Define procedure cubic that can be used with newton's method
;; to approx zeros of cubic x^3 + ax^2 + bx + c
(define (cubic a b c)
  (lambda (x)
    (+ (* x x x)
         (* a x x)
         (* b x)
         c)
    ))

(newtons-method (cubic 0 0 5) 1)

;; ex 1.41
(define (double proc)
  (lambda (x)
    (proc (proc x))))

(define (inc x) (+ 1 x))
;; g := double (double f)
;; (g (g f))
;; f := inc
;; double(f) := inc2
;; (inc2 (inc2 x)) => inc4x, so inc16 is the final result
(((double (double double)) inc) 5)

;; ex 1.42
(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

;; ex 1.43
(define (repeated f n)
  (define (repeat-iter g i)
    (if (= i 0) g
      (repeat-iter (compose f g) (- i 1))))
  (lambda (x) ((repeat-iter f (- n 1)) x)))
(displayln "woo")
((repeated square 2) 5)

;; ex 1.44
(define (smooth f)
  (define (average x y z)
    (/ (+ x y z) 3))
  (lambda (x) (average 
                (f (- x dx))
                (f x)
                (f (+ x dx))))
  )

((n-smooth square 3) 5)

(define (n-smooth f n)
  (repeated (smooth f) n))
