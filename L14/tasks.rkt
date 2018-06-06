#lang racket

(define (cons x y)
  (define (set-x! v) (set! x v))
  (define (set-y! v) (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation -- CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)

(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)


(define (make-cycle mxs)
  (define (f e)
    (if (null? (mcdr e))
        (set-mcdr! e mxs)
        (f (mcdr e))))
  (f mxs))

(define (has-cycle? xs)
  (define (nth i xs)
    (define (traverse iter xs)
      (if (null? xs) null
          (if (= i iter)
              (mcar xs)
              (traverse (+ 1 iter) (mcdr xs)))))
    (traverse 1 xs))
  
  (define (member-cntr uBound zs elem iter)
    (if (>= iter uBound)
        #f
        (if (eq? (nth iter zs) elem)
            #t
            (member-cntr uBound zs elem (+ 1 iter)))))

  (define (f cntr)
    (if (null? (nth cntr xs))
        #f
        (or (member-cntr cntr xs (nth cntr xs) 1)
            (f (+ 1 cntr)))))
  
  (f 1))