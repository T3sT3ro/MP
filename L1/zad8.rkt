#lang racket

(define (power-close-to b n)
  (define (iter e)
    (if (> (expt b e) n)
        e
        [iter (add1 e)]
    )
  )
  (iter 1)
)