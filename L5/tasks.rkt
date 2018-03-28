#lang racket

;; arithmetic expressions

(define (const? t)  (number? t))
(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)  (car e))
(define (binop-left e)  (cadr e))
(define (binop-right e)  (caddr e))
(define (binop-cons op l r)  (list op l r))

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

;; calculator

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-arith e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-arith (binop-left  e))
            (eval-arith (binop-right e)))]))

;; let expressions

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
(define (let-expr e) (caddr e))
(define (let-cons def e)  (list 'let def e))

(define (var? t) (symbol? t))
(define (var-var e) e)
(define (var-cons x) x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith/let-expr? (binop-left  t))
           (arith/let-expr? (binop-right t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def (let-def-expr t))))
      (var? t)
      (and (if-zero? t)
           (arith/let-expr? (if-zero-cond t))
           (arith/let-expr? (if-zero-true t))
           (arith/let-expr? (if-zero-false t)))))

;; if-zero
(define (if-zero-cons cond true false) (list 'if-zero cond true false))
(define (if-zero? t)
  (and (list? t)
       (= (length t) 4)
       (eq? (car t) 'if-zero)))

(define (if-zero-cond t)  (cadr t))
(define (if-zero-true t)  (caddr t))
(define (if-zero-false t)  (cadddr t))



;; evalation via substitution

;; Expresion Xsubstitutet-var Fsubstitution-var
(define (subst e x f)
  (cond [(const? e) e]
        [(binop? e)
         (binop-cons
           (binop-op e)
           (subst (binop-left  e) x f)
           (subst (binop-right e) x f))]
        [(let? e)
         (let-cons
           (let-def-cons
             (let-def-var (let-def e))
             (subst (let-def-expr (let-def e)) x f))
           (if (eq? x (let-def-var (let-def e)))
               (let-expr e)
               (subst (let-expr e) x f)))]
        [(var? e)
         (if (eq? x (var-var e))
             f
             (var-var e))]
        [(if-zero? e)
         (if-zero-cons
          (subst (if-zero-cond e))
          (subst (if-zero-true e))
          (subst (if-zero-false e)))]))

(define (eval-subst e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-subst (binop-left  e))
            (eval-subst (binop-right e)))]
        [(let? e)
         (eval-subst
           (subst
             (let-expr e)
             (let-def-var (let-def e))
             (eval-subst (let-def-expr (let-def e)))))]
        [(var? e)
         (error "undefined variable" (var-var e))]
        [(if-zero? e)
         (if [= (eval-subst (if-zero-cond e)) 0]
             (eval-subst (if-zero-true e))
             (eval-subst (if-zero-false e)))]))

;; evaluation via environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (eval-env e env)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(let? e)
         (eval-env
           (let-expr e)
           (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]
        [(if-zero? e)
         (if [= (eval-env (if-zero-cond e) env) 0]
             (eval-env (if-zero-true e) env)
             (eval-env (if-zero-false e) env))]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

(define (eval e)
  (eval-env e empty-env))

;; to RPN
(define (arith->rpn e)
  (define (rec e l)
    (cond [(const? e) (cons e l)]
          [(binop? e) (rec (binop-left e) (rec (binop-right e) (cons (binop-op e) l)))]))
  (if (not (arith-expr? e))
      (error "Not an arith-expr.")
      (rec e null)))

;; stack struct
(define (stack? s)
  (and (list? s)
       (eq? (car s) 'stack)))

(define (push val stack)
  (if [not (stack? stack)]
      (error "Not a stack.")
      (cons 'stack (cons val (cdr stack)))))

(define (pop stack)
  (if [not (stack? stack)]
      (error "Not a stack.")
      (cons (cadr stack) (cons 'stack (cddr stack)))))

;; RPN calc
(define (eval-rpn rpn)
  (define (f rpn stack)
    (cond [(null? rpn) (pop stack)]
          [(const? (car rpn)) (f (cdr rpn) (push (car rpn) stack))]
          [else (f (cdr rpn) (push
                              ((op->proc (car rpn))
                               (car (pop (cdr (pop stack))))
                               (car (pop stack))) (cdr (pop ( cdr (pop stack))))))]))
  (car (f rpn '(stack))))












