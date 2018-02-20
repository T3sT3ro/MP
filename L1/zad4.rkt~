#lang racket

(define (_sq_sum x y) (+ (* x x) (* y y)))
(define (big_sq a b c)
  (cond
    [(= (min a b c) c) (_sq_sum a b)]
    [(= (min a b c) b) (_sq_sum a c)]
    [(= (min a b c) a) (_sq_sum b c)]
  )
)