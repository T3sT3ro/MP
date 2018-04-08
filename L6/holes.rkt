#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

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

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  (define (get-context e local-context)
    (if [hole? e]
        local-context
        (cond [(const? e) null]
              [(binop? e) (append (get-context (binop-left e) local-context)
                                  (get-context (binop-right e) local-context))]
              [(let? e) (append (get-context (let-def-expr (let-def e)) local-context)
                                (get-context (let-expr e) (remove-duplicates (append local-context
                                                                                     (list (let-def-var (let-def e)))))))]
              [(var? e) null])))
  (if [arith/let/hole-expr? e]
      (remove-duplicates (get-context e null))
      (error "Invalid expression with hole"))
  )

(define (test)
  ;; compares two lists element by element
  (define (list-eq? cx1 cx2)
    (let ([c1 (sort cx1 symbol<?)]
          [c2 (sort cx2 symbol<?)])
      (or (and (null? c1) (null? c2))
           (and (list? c1) (list? c2)
                (= (length c1) (length c2))
                (eq? (car c1) (car c2))
                (list-eq? (cdr c1) (cdr c2))))))
  (and (list-eq? '() (hole-context '(+ 3 hole)))
       (list-eq? '(x y) (hole-context '(let (x 3) (let (y 7) (+ x hole)))))
       (list-eq? '(x) (hole-context '(let (x 3) (let (y hole) (+ x 3)))))
       (list-eq? '() (hole-context '(let (x hole) (let (y 7) (+ x 3)))))
       (list-eq? '(piesek kotek chomik)
                 (hole-context '(let (piesek 1) (let (kotek 7) (let (piesek 9) (let (chomik 5) hole))))))
       (list-eq? '() (hole-context '(+ (let (x 4) 5) hole)))
       (list-eq? '(x) (hole-context '(+ (- 1 2) (let (x 3) (let (x 2) hole))))))
  )
