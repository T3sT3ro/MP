#lang racket

;;
;; author Tooster
;;

(define (compose f g) (lambda (x) (f (g x))))

(define (repeated p n)
        (if (= n 0)
            [identity p]
            [compose (identity p) (repeated p (sub1 n)) ]
        )
)

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2)))

(define (fix-point f x0)
        (let ([x1 (f x0)])
             (if [< (abs (- x0 x1)) 0.00001]
                 x0
                 (fix-point f x1)
             )
        )
)


(define (nth-root x n)
        {fix-point ((repeated average-damp (floor (log n 2)))
                    (lambda (y) (/ x (expt y (sub1 n))))
                   )
                   1.0
        }
)

;; tests

(nth-root 2 1) ;; = 2
(nth-root 2 2) ;; = 1.41
(nth-root 3 1) ;; = 3
(nth-root 4 2) ;; = 2
(nth-root -27 3) ;; = -3
(nth-root 8 3) ;; = 2
(nth-root 1024 10) ;; = 2
(nth-root 125 3) ;; = 5