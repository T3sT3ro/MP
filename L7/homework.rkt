#lang racket

;; expressions

(define (const? t)  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

(define (op-op e)  (car e))
(define (op-args e)  (cdr e))
(define (op-cons op args)  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)  (car e))
(define (let-def-expr e)  (cadr e))
(define (let-def-cons x e)  (list x e))

(define (let? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
       (let-def? (cadr t))))

(define (let-def e)  (cadr e))
(define (let-expr e)  (caddr e))
(define (let-cons def e)  (list 'let def e))

(define (var? t)  (symbol? t))
(define (var-var e)  e)
(define (var-cons x)  x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env null)
(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; evaluator

(define (eval-env e env)
  (cond [(const? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]))

(define (env-for-let def env)
  (add-to-env
   (let-def-var def)
   (eval-env (let-def-expr def) env)
   env))

(define (eval e)
  (eval-env e empty-env))

;; let-lift procedure
(define (let-lift e)
  ;; intermediate structure as (arith-expr, definitions from lets, newest counter for symbol renaming)
  (define (ret-cons expr defs cntr) (list expr defs cntr))
  (define (ret-expr t) (first t))
  (define (ret-defs t) (second t))
  (define (ret-cntr t) (third t))
  
  ;; wraps arith-expr into lets in order as in defs (left is outer definition)
  (define (wrap expr defs)
    (if [null? defs] expr (let-cons (car defs) (wrap expr (cdr defs)))))

  ;; transforms list of op-args into ret as (list of rets from subexprs., definitions, newest counter)
  (define (arith/let-list->ret-list xs cntr env)
    (if [null? xs]
        (ret-cons null null cntr)
        (let* ([ret (arith/let->ret (car xs) cntr env)]
               [xs-ret (arith/let-list->ret-list (cdr xs) (ret-cntr ret) env)])
          (ret-cons (cons (ret-expr ret) (ret-expr xs-ret)) ;; expr will be list of ret's 
                    (append (ret-defs ret) (ret-defs xs-ret)) ;; concat definitions in disjoint subexpressions
                    (ret-cntr xs-ret)))))
  
  ;; tranforms to intermediate result structure: res
  (define (arith/let->ret e cntr env) ;; produces ret struct which has arith expressions, list of definitions and cntr for new symbol
    (cond [(const? e) (ret-cons e null cntr)]
          [(var? e) (ret-cons (find-in-env e env) null cntr)]
          [(op? e) (let ([ret (arith/let-list->ret-list (op-args e) cntr env)]) ;; process op-args
                     (ret-cons (op-cons (op-op e) (ret-expr ret))
                               (ret-defs ret)
                               (ret-cntr ret)))]
          [(let? e) (let* ([def-ret (arith/let->ret (let-def-expr (let-def e)) cntr env)] ;; proper return from definitions expr
                           [current-cntr (ret-cntr def-ret)] ;; new available counter for variables
                           [expr-ret (arith/let->ret (let-expr e)
                                                     (add1 current-cntr)
                                                     (add-to-env (var-var (let-def-var (let-def e)))
                                                                 (number->symbol current-cntr)
                                                                 env))]);; return from expr
                      (ret-cons (ret-expr expr-ret) ;; let expresison as ret-expr
                                (append (ret-defs def-ret) ;; append definitions from def expr, def var and expr
                                        (list (let-def-cons (number->symbol current-cntr) (ret-expr def-ret))) ;; let's definition
                                        (ret-defs expr-ret))
                                (ret-cntr expr-ret)))])) ;; update counter after this let
  (if [arith/let-expr? e]
      (let ([ret (arith/let->ret e 0 empty-env)])
        (wrap (ret-expr ret) (ret-defs ret))) ;; wrap arith-expr into lets according to defs
      (error "Malformed arith/let-expr expression")))

;; test generation;

(define (gen-expression max-depth)
  (define (gen depth bound)
    (let* ([r1 (random 100)]
           [r1 (if [= depth 1] (modulo r1 20) r1)])
      (cond [(< r1 5) (random 100)] ;; 5% chance 
            [(< r1 30) (if [null? bound] ;; 15 % chance
                           (random 100)
                           (var-cons (car (shuffle bound))))]
            [(< r1 50) (cons (car (shuffle '(+ - *))) ;; without '/ to avoid zero division
                             (build-list (random 2 5) ;; branching factor: [2 4]
                                         (lambda (x) (gen (sub1 depth) bound))))]
            [else (let ([r2 (number->symbol (random 1 4))])
                    (let-cons (let-def-cons r2 (gen (sub1 depth) bound))
                              (gen (sub1 depth)  (remove-duplicates (cons r2 bound)))))])))
  (gen max-depth null))

;; test calculation

(define (test-expr e)
  (if (let* ([lifted (let-lift e)])
        (and (let-lifted-expr? lifted)
             (= (eval e) (eval lifted))))
      #t
      (printf "failed: ~a\n" e)))

(display "Fixed test pass status: ")
(and (test-expr '(let (x (let (x 2) (+ x (let (y x) (let (x 3) (* x y))))))
                   (let (x 3) (let (y 5) (+ (let (x 2) (+ x y)) (let (x 1) (+ x y)))))))
     (test-expr '(let (x1 1) (let (x2 2) (+ x1 x2)) ))
     (test-expr '(let (x1 1) (+ x1 (let (x2 x1) (let (x1 7) (+ x1 x2))))))
     (test-expr '(let (x 3) (+ x (let (x 7) x)))))

(define (test)  
  (printf "Test 1000 random expressions: ~a\n"
          (andmap test-expr (build-list 1000 (lambda (x) (gen-expression (random 1 10)))))))