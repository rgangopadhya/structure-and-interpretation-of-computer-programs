; What is the issue with "new-if"... why does `if` need to be provided as a special form
; instead of an implementation of cond
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))

(define (square x)
  (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

; using if here gives an answer
; using new-if results in an infinite loop?
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x)
                     x)))

(define (sqrt x)
  (sqrt-iter 1.0 x))

; 1.2
(/ 
  (+ 5
     4 
     (- 2 
        (- 3 
           (+ 6 
              (/ 4 5)))))
  (* 3
     (- 6 2)
     (- 2 7))
)

; 1.3
(define (squared-distance x y)
  (+ (* x x) (* y y)))

; x < y and x < z: x can't be in the top 2
; otherwise, x must be in the top 2
(define (greater-sum-squares x y z)
  (cond ((and (< x y) (< x z)) (squared-distance y z))
        (else (squared-distance x (max y z)))))

; 1.7: the good-enough? test not very effective for
; finding sqrt of small numbers. Also, limited precision
; so, also inadequate for very large numbers
; explain these statements, with examples showing how the test fails for small and large numbers
; For very small numbers, e.g., 10^-6, the absolute error is quite small even for bad guesses.
; Suppose the guess is 10^-4, then the error, 10^-8 - 10^-6, is << 0.001. The true error is 
; (10^-8 - 10^-6) / 10^-6, or -99%!
; Likewise, for large numbers, limited precision is available, which means even very close
; guesses are rejected because the significant digits exceed the error threshold
; for example, for something close to the largest number in 32 bit float, 3.01 * 10^38,
; if we guess 1.7349 * 10^38, the error (-0.0001 * 10^38) vastly exceeds. Even if we make
; the guess as precise as possible, 1.7349351572897471 * 10^38, the error still vastly exceeds.

; so, the fix: stop when change is a very small fraction of the guess

(define (alt-sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))

  (define (good-enough? prev-guess next-guess)
    (< 
      (/ (abs (- prev-guess next-guess))
         prev-guess)
      0.0001))

  (define (sqrt-iter guess prev-guess)
    (if (good-enough? guess prev-guess)
            guess
            (sqrt-iter (improve guess)
                       guess)))

  (sqrt-iter 1.0 0.0))

; is this better for large and small numbers? based on ratio of the guesses
; so that handles small and large numbers better. Yes!

; ex 1.9
(define (+ a b)
  (if (= a 0) b (+ (dec a) (inc b))))

; substitute:
(+ 4 5)
   (if (= 4 0) 5 (+ (dec 4) (inc 5)))
   (+ (3) (6))
   (if (= 3 0) 6 (+ (dec 3) (inc 6)))
   (+ 2 7)
   (if (= 2 0) 7 (+ (dec 2) (inc 7)))
   (+ 1 8))
   (if (= 1 0) 8 (+ (dec 1) (inc 8)))
   (+ 0 9))
   (if (= 0 0) 9 (+ (dec 0) (inc 9)))
  9

; ex 1.12

(define (pascal row column)
  (define (is-boundary row column)
    (or (= row 0)
        (= row 1)
        (= column 0)
        (= column row))
  )
  (cond ((is-boundary row column) 1)
        (else (+ (pascal 
                   (- column 1)
                   (- row 1))
                 (pascal
                   column
                   (- row 1)
                  )))))
