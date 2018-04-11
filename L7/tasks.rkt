#lang racket
(require racket/trace)

;; void
(define (void? t) (eq? t 'void))
(define (void-cons) 'void)

;; boolean
(define (bool? t)
  (or (eq? t 'true)
      (eq? t 'false)))

(define (true-cons) 'true)
(define (false-cons) 'false)
(define (true? t) (eq? t 'true))
(define (false? t) (eq? t 'false))

;; if
(define (if? t)
  (and (list? t)
       (eq? (car t) 'if)
       (= (length t) 4)))

(define (if-cons predicate true-expr false-expr)
  (list 'if predicate true-expr false-expr))
(define (if-pred e) (second e))
(define (if-true-expr e) (third e))
(define (if-false-expr e) (fourth e))

;; cond
(define (cond? t)
  (and (list? t)
       (eq? (car t) 'cond)
       (> (length t) 1)))

(define (cond-case? t)
  (and (list? t)
       (= (length t) 2)))

(define (cond-case-pred t) (first t))
(define (cond-case-expr t) (second t))
(define (else? t) (eq? t 'else))

(define (cond>if t)
  (define (f xs)
    (if [null? xs]
        (void-cons)
        (if [else? (cond-case-pred (car xs))]
            (if-cons true (cond-case-expr (car xs)) (void-cons))
            (if-cons (cond-case-pred (car xs)) (cond-case-expr (car xs)) (f (cdr xs))))))
  (f (cdr t)))

;; null
(define (null-null? t)
  (eq? t 'null))

(define (null-cons) 'null)

;; list
(define (list-list? t)
  (and (list? t)
       (eq? (car t) 'list)))

(define (list>cons e)
  (define (f xs)
    (if [null? xs]
        (null-cons)
        (cons-cons (car xs) (f (cdr xs)))))
    (f (cdr e)))

;; arithmetic expressions
(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / < > <= >=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '<) <]
        [(eq? op '>) >]
        [(eq? op '<=) <=]
        [(eq? op '>=) >=]))

;; lets

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

;; variables

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

;; pairs

(define (cons? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'cons)))

(define (cons-fst e)
  (second e))

(define (cons-snd e)
  (third e))

(define (cons-cons e1 e2)
  (list 'cons e1 e2))

(define (car? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'car)))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'cdr)))

(define (cdr-expr e)
  (second e))

;; lambdas

(define (lambda? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'lambda)
       (list? (cadr t))
       (andmap symbol? (cadr t))))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; applications

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

(define (app-proc e)
  (car e))

(define (app-args e)
  (cdr e))

;; expressions

(define (expr? t)
  (or (bool? t)
      (void? t)
      (and (if? t)
           (expr? (if-pred t))
           (expr? (if-true-expr t))
           (expr? (if-false-expr t)))
      (and (cond? t)
           (andmap (lambda (x) (and (cond-case? x)
                                    (or (expr? (cond-case-pred x))
                                        (else? (cond-case-pred x)))
                                    (expr? (cond-case-expr x))))
                   (cdr t)))
      (null-null? t)
      (and (list-list? t)
           (andmap expr? (cdr t)))
      (const? t)
      (and (op? t)
           (andmap expr? (op-args t)))
      (and (let? t)
           (expr? (let-expr t))
           (expr? (let-def-expr (let-def t))))
      (var? t)
      (and (cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))))

;; environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; closures

(define (closure-cons xs expr env)
  (list 'closure xs expr env))

(define (closure? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'closure)))

(define (closure-vars c)
  (cadr c))

(define (closure-expr c)
  (caddr c))

(define (closure-env c)
  (cadddr c))

;; evaluator

(define (eval-env e env)
  (cond [(bool? e) (true? e)]
        [(void? e) (void)]
        [(if? e) (if [eval-env (if-pred e) env]
                     (eval-env (if-true-expr e) env)
                     (eval-env (if-false-expr e) env))]
        [(cond? e) (eval-env (cond>if e) env)]
        [(null-null? e) null]
        [(list-list? e) (eval-env (list>cons e) env)]
        [(const? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]
        [(cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(app? e)
         (apply-closure
           (eval-env (app-proc e) env)
           (map (lambda (a) (eval-env a env))
                (app-args e)))]))

(define (apply-closure c args)
  (eval-env (closure-expr c)
            (env-for-closure
              (closure-vars c)
              args
              (closure-env c))))

(define (env-for-closure xs vs env)
  (cond [(and (null? xs) (null? vs)) env]
        [(and (not (null? xs)) (not (null? vs)))
         (add-to-env
           (car xs)
           (car vs)
           (env-for-closure (cdr xs) (cdr vs) env))]
        [else (error "arity mismatch")]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))