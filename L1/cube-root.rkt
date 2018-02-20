#lang racket

(define (cube x) (* x x x))


(define (cube-root x)
  ;; iterates over approximation
  (define (approximate approx)
    (if (< (abs (- x (cube approx))) (cube 0.001))
        approx
        [approximate (improve approx)]))

  ;; improves approximation
  (define (improve approx)
    (/ (+ (/ x (* approx approx)) (* 2 approx)) 3))
  
  (approximate 1.0)
)