#lang racket

(define (p) (p))
(define (test x y)
              (if (= x 0)
                  0
                  y))
; jakby wykonywał leniwie to by wywołało 0