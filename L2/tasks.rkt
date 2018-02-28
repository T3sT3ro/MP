#lang racket

(define (compose f g) (lambda (x) (f (g x))))

(define (square x) (* x x))
(define (inc x) (add1 x))

(define (repeated p n)
  (if (= n 0)
      [identity p]
      [compose (identity p) (repeated p (sub1 n)) ]
  )
)

;; product calculated recursively
(define product
  (lambda (term nextval start end)
    {if (> start end)
        0
        [* (term start) (product nextval (nextval start) end)]
    }
  )
)

;; product calculated iteratively
(define product-it
  (lambda (term nextval start end)
    (define (prod i ret)
       {if (= i end)
           (* (term i) ret)
           [prod (nextval i) (* (term i) ret)]
         
       }
    )
    {prod start 1}
  )
)

(define (ith-frac n) ;; 1-index based
  (/ (* (* 2 n) (* 2 (+ n 1)))
     (square (+ 1 (* 2 n))))
)


;;
;; ACCUMULATORS
;;

(define accumulate
  (lambda (combiner null-value term a next b)
    {if (> a b)
        null-value
        [combiner (term a) (accumulate combiner null-value term (next a) next b)]
    }
  )
)

(define accumulate-it
  (lambda (combiner null-value term a next b)
    (define (acc i ret)
       {if (= i b)
           (combiner (term i) ret)
           [acc (next i) (combiner (term i) ret)]
         
       }
    )
    {acc a null-value}
  )
)

;;
;; RECURSIVE FRACTIONS
;;

(define (cont-frac num den k)
  (define (recur curr)
    {if (> curr k)
        0
        [/ (num curr)
           (+ (den curr) (recur (add1 curr)))]
    }
  )
  {recur 0}
)

;; PI (+ 3 ( cont-frac ( lambda ( i ) (square (+ 1 (* 2 i)))) ( lambda ( i ) 6.0) 100 ))

(define (atan-cf x k)
  (define (at-nom n)
    {if (= n 0)
        x
        (square (* n x))
     })
  (define (at-den n) {+ 1 (* 2 n)})
  {cont-frac at-nom at-den k}
)
