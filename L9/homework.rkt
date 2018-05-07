#lang racket

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;;
;; WHILE
;;

; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bool expressions: syntax and semantics

(define (const? t)
  (number? t))

(define (true? t)
  (eq? t 'true))

(define (false? t)
  (eq? t 'false))

(define (rand? t)
  (tagged-tuple? 'rand 2 t))

(define (rand-arg t)
  (second t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= not and or mod))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]
        [(eq? op 'not) not]
        [(eq? op 'and) (lambda x (andmap identity x))]
        [(eq? op 'or) (lambda x (ormap identity x))]
        [(eq? op 'mod) modulo]))

(define (var? t)
  (symbol? t))


(define (eval-arith e m)  
  (define (eval-arith-args args s)
    (if (null? args)
        (res null s)
        (let* ([r ((eval-arith (car args) m) s)]
               [filtered (eval-arith-args (cdr args) (res-state r))])
          (res (cons (res-val r) (res-val filtered))
               (res-state filtered)))))
  
  (cond [(true? e) (curry res true)]
        [(false? e) (curry res false)]
        [(var? e) (curry res (get-mem e m))]
        [(op? e) (lambda (s)
                   (let ([args (eval-arith-args (op-args e) s)])
                     (res (apply (op->proc (op-op e)) (res-val args))
                          (res-state args))))]
        [(const? e) (curry res e)]
        [(rand? e) (lambda (s)
                     (let ([r ((eval-arith (rand-arg e) m) s)])
                       ((rand (res-val r)) (res-state r))))])) ;;(let ([r (eval-arith (rand-arg e) m s)])
;;((rand (res-val r)) (res-state r)))]))

;; syntax of commands

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (block? t)
  (list? t))

;; state

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;; psedo-random generator

(define initial-seed
  123456789)

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

;; WHILE interpreter

(define (old-eval-arith e m)
  (cond [(true? e) true]
        [(false? e) false]
        [(var? e) (get-mem e m)]
        [(op? e)
         (apply
          (op->proc (op-op e))
          (map (lambda (x) (old-eval-arith x m))
               (op-args e)))]
        [(const? e) e]))

(define (old-eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (old-eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (old-eval-arith (if-cond e) m)
             (old-eval (if-then e) m)
             (old-eval (if-else e) m))]
        [(while? e)
         (if (old-eval-arith (while-cond e) m)
             (old-eval e (old-eval (while-expr e) m))
             m)]
        [(block? e)
         (if (null? e)
             m
             (old-eval (cdr e) (old-eval (car e) m)))]))

(define (eval e m seed)
  (define (eval-st e m seed)
    (cond [(assign? e)
           (let ([r ((eval-arith (assign-expr e) m) seed)])
             (res (set-mem
                   (assign-var e)
                   (res-val r)
                   m)
                  (res-state r)))]
          [(if? e)
           (let ([r ((eval-arith (if-cond e) m) seed)])
             (if (res-val r)
                 (eval-st (if-then e) m (res-state r))
                 (eval-st (if-else e) m (res-state r))))]
          [(while? e)
           (let ([r ((eval-arith (while-cond e) m) seed)])
             (if (res-val r)
                 (let ([rx (eval-st (while-expr e) m (res-state r))])
                   (eval-st e (res-val rx) (res-state rx)))
                 (res m (res-state r))))]
          [(block? e)
           (if (null? e)
               (res m seed)
               (let ([r (eval-st (car e) m seed)])
               (eval-st (cdr e) (res-val r) (res-state r))))]))
  (res-val (eval-st e m seed)))

(define (run e)
  (eval e empty-mem initial-seed))

;;

(define fermat-test 
  '{{i := 0}
    {composite := (= n 1)}
    {while (and (> n 3) (not composite) (< i k)) {
         {x := (+ 2 (rand (- n 3)))}
         {y := 1}
         {e := (- n 1)}
         {while (> e 1) { ;; r ^ n-1
              {if (= (mod e 2) 0)
                 {{x := (* x x)} {e := (/ e 2)}}
                 {{y := (* x y)} {x := (* x x)} {e := (/ (- e 1) 2)}}}}}
         {if (= (mod (* x y) n) 1) {} {composite := true}}
         {i := (+ 1 i)}}}
    }
  )

(define (probably-prime? n k) ; check if a number n is prime using
  ; k iterations of Fermat's primality
  ; test
  (let ([memory (set-mem 'k k
                         (set-mem 'n n empty-mem))])
    (not (get-mem
          'composite
          (eval fermat-test memory initial-seed)))))

(define (primes n k)
    (filter (lambda (n) (probably-prime? n k))
            (build-list n (compose identity add1))))

(require math/number-theory)
(define (test n k) (andmap (lambda (x)
                             (or (probably-prime? x k)
                                 (and (not (probably-prime? x k)) (not (prime? x)))))
                           (primes n k)))