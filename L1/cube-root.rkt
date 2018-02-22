#lang racket

;; author Tooster aka Maksymilian Polarczyk

;; returns x to the power of three
(define (cube x) (* x x x))

;; calculates approximation of cube root from x
(define (cube-root x)
  ;; precision of rounding
  (define precission 0.001)
  ;; iterates over approximation
  (define (approximate approx)
    (if (< (abs (- x (cube approx))) (cube precission))
        approx
        [approximate (improve approx)]))

  ;; improves approximation
  (define (improve approx)
    (/ (+ (/ x (* approx approx)) (* 2 approx)) 3))

  ;; for negative do the same reversing the sign
  (if (< x 0)
      [* -1 (cube-root (* -1 x))]
      [approximate 1.0]
  )
)

;; tests
(cube-root 0)
(cube-root 8)
(cube-root 9)
(cube-root 27)
(cube-root 81)
(cube-root 125)
(cube-root 1000)
(cube-root 0.125)
(cube-root 0.027)
(cube-root -8)
(cube-root -3)
(cube-root -27)
(cube-root -1)
(cube-root 1)
(cube-root -125)
(cube-root -1000) 
