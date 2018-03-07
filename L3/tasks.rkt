#lang racket

;; Z1

(define (make-rat n d)
        {let ([c (gcd n d)])
             {list (/ n c) (/ d c)}
        }
)

(define (rat-num L)
        {car L})

(define (rat-den L)
        {car (cdr L)})

(define (rat? L)
        {and [list? L]
             [not (= (rat-den L) 0)]
             [= 1 (gcd (rat-num L) (rat-den L))]
        }
)

;; Z2
;;=====
(define (make-point x y)
        {cons x y})
;;
(define (point-x P)
        {car P})

(define (point-y P)
        {cdr P})
;;
(define (point? P)
        {pair? P})
;;=====

;;=====
(define (make-vect P1 P2)
        {cons P1 cons P2})
;;
(define (vect-begin V)
        {car V})

(define (vect-end V)
        {cdr V})
;;
(define (vect? V)
        {and [point? (vect-begin V)]
             [point? (vect-end V)]})
;;=====
(define (vect-length V)
        {sqrt (+ (expt (- (point-x (vect-end)) (point-x (vect-begin))) 2)
                 (expt (- (point-y (vect-end)) (point-y (vect-begin))) 2))
        }
)

(define (vect-scale V k)
        {let ([P1 (vect-begin V)]
              [P2 (vect-end V)])
          
             (make-vect P1 (make-point (- (* k (point-x P2)) (* k (point-x P1)))
                                       (- (* k (point-y P2)) (* k (point-y P1)))))
        }
)

(define (vect-translate V T)
        {let ([dx (- (point-x (vect-end T)) (point-x (vect-begin T)))]
              [dy (- (point-y (vect-end T)) (point-y (vect-begin T)))]
              [P1 (vect-begin V)]
              [P2 (vect-end V)])

             (make-vect (make-point (+ dx (point-x P1)) (+ dy (point-y P1)))
                        (make-point (+ dx (point-x P2)) (+ dy (point-y P2))))
        }
)
;;=====

;; Z4

(define (reverse L)
        (define (build list reversed)
                {if [null? list]
                    reversed
                    (build (cdr list) (cons (car list) reversed))}
        )
        {build L null}
)