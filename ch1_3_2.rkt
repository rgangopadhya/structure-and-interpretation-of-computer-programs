#!/usr/bin/racket
#lang racket

;; recall
;; (define (pi-sum a b)
;;   (define (pi-term x)
;;     (/ 1.0 (* x (+ x 2))))
;;   (define (pi-next x)
;;     (+ x 4))
;;   (sum pi-term a pi-next b))
;; awkward to define pi-term and pi-next just to use them later

(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

(display (* 8 (pi-sum 1 10000)))

;; and, compare
;; (define (integral f a b dx)
;;   (define (add-dx x)
;;     (+ x dx))
;;   (* (sum f (+ a (/ dx 2.0)) add-dx b)
;;      dx))
;; to
(define (integral f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;; (lambda (<formal-parameters>) <body>)
;; the difference: no name in the environment

;; Using let to create local variables
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; (let ((<var1> <exp1>)
;;       (<var2> <exp2>))
;;       <body>)
;;  Note that a let expression is just syntactic sugar for the underlying lambda application:
;;  ((lambda (<var1> ... <varn>)
;;      <body>)
;;   <exp1>
;;   ...
;;   <expn>)

;; Exercise 1.34
;; Suppose we define
(define (f g) (g 2))
(displayln (f (lambda (x) (* x x))))
(displayln (f (lambda (z) (* (+ z 1) z))))
;; what happens if we do (f f)?
;; substitute: (f f) => (f 2) => error since '2' is not a procedure

;; 1.3.3 Procedures as General Methods
;; Ex 1.35
;; Show that the golden ratio (1 + sqrt(5) ) / 2, satisfies phi**2 = phi + 1
;; phi = 1 + 1 / phi
;; thus, a fixed point of x -> 1 + 1/x
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (newline)
      (displayln next)
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
;; (displayln (fixed-point cos 1.0))
(displayln "estimating phi")
(displayln (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))
;; (displayln (fixed-point (lambda (x) (/ (log 1000) (log x))) 2))

;; takes 35 steps
;; with average damping, which means take
;; the original function and look for a fixed
;; point in the average instead?
(define (average x1 x2) (/ (+ x1 x2) 2))
(displayln "estimating x^x")
(displayln (fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2))
;; takes 10 steps instead! whoa
;; ex 1.37
(define (cont-frac n d k)

  (display k)
  (display "  ")
  (display (n k))
  (display "  ")
  (display (d k))
  (display "  ")
  (display (/ (n k) (d k)))
  (newline)
  (if (= k 0)
    0
    (/
      (n k)
      (+ (d k)
         (cont-frac n d (- k 1))))))
(displayln "estimating 1/phi using finite continued fraction")
(displayln (/ 1 (cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)))

;; ex 1.38
;; continued fraction expansion for e - 2
;; N_i: all i
;; D_i: 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8...
;;      0  1  2  3  4  5  6  7  8  9  10
;;         1        4        7        10
;;         modulo (i - 1) 3 = 0
(displayln "estimating e-2")
(displayln ((lambda (i)
                          (cond
                            ((= i 0) 1) 
                            ((= 0 (modulo (- i 1) 3.0))
                                1)
                            (else (* 2.0 (ceiling (/ i 3.0))
                            )))) 0))
(displayln (+
             2
             (cont-frac (lambda (i) 1.0)
                        (lambda (i)
                          (cond
                            ((= i 0) 1) 
                            ((= 0 (modulo (- i 1) 3.0))
                                1)
                            (else (* 2.0 (ceiling (/ i 3.0))
                            )))) 10000)))
