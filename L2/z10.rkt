#lang racket

;;
;; author Tooster
;;

(define (frac nom den error) ;; nom and den should produce 1-index based next nominator and denominator series
        (define (rec n A-prev A-curr B-prev B-curr)
               {let* ([n_next (add1 n)] ;; next n
                      [A-next (+ (* (den n_next) A-curr) (* (nom n_next) A-prev))]
                      [B-next (+ (* (den n_next) B-curr) (* (nom n_next) B-prev))]
                      [F-curr (/ A-curr B-curr)] ;; f_n
                      [F-next (/ A-next B-next)]) ;; f_n+1

                     
                     (if [< (abs (- F-curr F-next)) error]
                         F-next ;; F-next has smaller error than F-curr
                         (rec n_next A-curr A-next B-curr B-next)
                     )
               }
         )
        ;; iter from n=0 as long as error is too big
        {rec 0 1 0 0 1}
)


;; tests
(define (square x) (* x x))

;; phi = 0.6180...
(* (frac (lambda (x) 1) (lambda (x) 1) 0.1) 1.0)
(* (frac (lambda (x) 1) (lambda (x) 1) 0.01) 1.0)
(* (frac (lambda (x) 1) (lambda (x) 1) 0.001) 1.0)

;; atan(1/2) = 0.4636...
(define (at-nom n)   {if (= n 1) x (square (* (sub1 n) x))}) ;; atan nominator series 1-indexed
(define (at-den n) {+ 1 (* 2 (sub1 n))}) ;; atan denominator series 1-indexed
(define x 1000) ;; for atan

(* (frac at-nom at-den 0.1) 1.0)
(* (frac at-nom at-den 0.01) 1.0)
(* (frac at-nom at-den 0.001) 1.0)

;; PI = 3.1415...
(define (PI-nom n) {square (+ 1 (* 2 (sub1 n)))})
(define (PI-den n) 6.0 )
(+ 3 (frac PI-nom PI-den  0.1))
(+ 3 (frac PI-nom PI-den  0.01))
(+ 3 (frac PI-nom PI-den  0.001))