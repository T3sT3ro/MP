#lang racket

(require "calc.rkt")

(define (def-name p)
  (car p))

(define (def-prods p)
  (cdr p))

(define (rule-name r)
  (car r))

(define (rule-body r)
  (cdr r))

(define (lookup-def g nt)
  (cond [(null? g) (error "unknown non-terminal" g)]
        [(eq? (def-name (car g)) nt) (def-prods (car g))]
        [else (lookup-def (cdr g) nt)]))

(define parse-error 'PARSEERROR)

(define (parse-error? r) (eq? r 'PARSEERROR))

(define (res v r)
  (cons v r))

(define (res-val r)
  (car r))

(define (res-input r)
  (cdr r))

;;

(define (token? e)
  (and (list? e)
       (> (length e) 0)
       (eq? (car e) 'token)))

(define (token-args e)
  (cdr e))

(define (nt? e)
  (symbol? e))

;;

(define (parse g e i)
  (cond [(token? e) (match-token (token-args e) i)]
        [(nt? e) (parse-nt g (lookup-def g e) i)]))

(define (parse-nt g ps i)
  (if (null? ps)
      parse-error
      (let ([r (parse-many g (rule-body (car ps)) i)])
        (if (parse-error? r)
            (parse-nt g (cdr ps) i)
            (res (cons (rule-name (car ps)) (res-val r))
                 (res-input r))))))

(define (parse-many g es i)
  (if (null? es)
      (res null i)
      (let ([r (parse g (car es) i)])
        (if (parse-error? r)
            parse-error
            (let ([rs (parse-many g (cdr es) (res-input r))])
              (if (parse-error? rs)
                  parse-error
                  (res (cons (res-val r) (res-val rs))
                       (res-input rs))))))))

(define (match-token xs i)
  (if (and (not (null? i))
           (member (car i) xs))
      (res (car i) (cdr i))
      parse-error))

;;

(define num-grammar
  '([digit {DIG (token #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)}]
    [numb {MANY digit numb}
          {SINGLE digit}]))

(define (node-name t)
  (car t))

(define (c->int c)
  (- (char->integer c) (char->integer #\0)))

(define (walk-tree-acc t acc)
  (cond [(eq? (node-name t) 'MANY)
         (walk-tree-acc
          (third t)
          (+ (* 10 acc) (c->int (second (second t)))))]
        [(eq? (node-name t) 'SINGLE)
         (+ (* 10 acc) (c->int (second (second t))))]))

(define (walk-tree t)
  (walk-tree-acc t 0))

;;
;; must rotate divs and minus afterwards
(define arith-grammar
  (append num-grammar
     '([add-expr {ADD-MANY   sub-expr (token #\+) add-expr}
                 {ADD-SINGLE sub-expr}]
       [sub-expr {SUB-MANY   mult-expr (token #\-) sub-expr}
                 {SUB-SINGLE mult-expr}]
       [mult-expr {MULT-MANY   div-expr (token #\*) mult-expr}
                  {MULT-SINGLE div-expr}]
       [div-expr {DIV-MANY   base-expr (token #\/) div-expr}
                 {DIV-SINGLE base-expr}]
       [base-expr {BASE-NUM numb}
                  {PARENS (token #\() add-expr (token #\))}])))

(define (arith-walk-tree t)
  (cond [(eq? (node-name t) 'ADD-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'SUB-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'MULT-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'DIV-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'ADD-MANY)
         (binop-cons
          '+
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'SUB-MANY)
         (binop-cons
          '-
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'MULT-MANY)
         (binop-cons
          '*
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'DIV-MANY)
         (binop-cons
          '/
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'BASE-NUM)
         (walk-tree (second t))]
        [(eq? (node-name t) 'PARENS)
         (arith-walk-tree (third t))]))

;; tree rotation
;;
;;   X             Y
;;  / \           / \
;; A   Y   ~~>   X   C
;;    / \       / \
;;   B   C     A   B
;;

;; applies rules of operator associativity to the syntax tree
(define (fix-tree t)
  ;; rules for tree rotation 
  (cond [(and (list? t) (or (and (or (eq? (node-name t) 'SUB-MANY)
                                     (eq? (node-name t) 'ADD-MANY))
                                 (or (eq? (node-name (fourth t)) 'SUB-MANY)
                                     (eq? (node-name (fourth t)) 'ADD-MANY)))
                            (and (or (eq? (node-name t) 'DIV-MANY)
                                     (eq? (node-name t) 'MULT-MANY))
                                 (or (eq? (node-name (fourth t)) 'DIV-MANY)
                                     (eq? (node-name (fourth t)) 'MULT-MANY)))))
         (fix-tree (list (node-name (fourth t)) ;;
               (list (node-name t)
                     (fix-tree (second t))
                     (third t) ;; symbol
                     (fix-tree (second (fourth t))))
               (third (fourth t)) ;; symbol
               (fix-tree (fourth (fourth t)))))]
        [(list? t) (cons (node-name t) (map fix-tree (cdr t)))]
        [else t]))

(define (calc s) (eval (arith-walk-tree (fix-tree (car (parse arith-grammar 'add-expr (string->list s)))))))


;; procedure printing syntax tree by Tooster - pass (car (parse arith-grammar 'add-expr "expr")) for it to work
(require pict/tree-layout)
(require pict)
(define (vis-tree expr)
  (define (f expr)
    (cond [(null? expr) null]
          [(list? expr) (apply tree-layout #:pict (colorize (text (symbol->string (car expr)) null 15) "chocolate") (map f (cdr expr)))]
          [else (tree-layout #:pict (colorize (text (if (symbol? expr) (symbol->string expr) (string expr)) (cons 'bold null) 20) "BLUE"))]))
  (naive-layered (f expr)))

;; Tests by Jakub Mendyk
(define (test)
  (display "Tests status: ")
  {and (= (exact->inexact (calc "74+4*36-(33/4-8)-26+97")) 288.7500)
       (= (exact->inexact (calc "(15*68/24)")) 42.5000)
       (= (exact->inexact (calc "9/(38+37)")) 0.1200)
       (= (exact->inexact (calc "49/50-(8*91)+14")) -713.0200)
       (= (exact->inexact (calc "(((19-40)-43)-2/40+(37-79))-(76+52-96+14*11)-(66-40+52-30)")) -340.0500)
       (= (exact->inexact (calc "14*88/38*19-86-69+94*94-99+60-(36-(99-61)+59+75*84)")) 2901.0000)
       (= (exact->inexact (calc "(34+4*81/(24/7/59))")) 5609.5000)
       (= (exact->inexact (calc "(75/25)+71+12-(28-100)")) 158.0000)
       (= (exact->inexact (calc "79-64*60*85*31/16*31")) -19604321.0000)
       (= (exact->inexact (calc "(22*85+100/12*54)")) 2320.0000)
       (= (exact->inexact (calc "1-2-3-4-5-6+7-8-9-10")) -39.0000)
       (= (exact->inexact (calc "1/2/2/2/2/2")) 0.03125)
       (= (exact->inexact (calc "1+2-3+4-5+6-7+8")) 6.0000)
       (= (exact->inexact (calc "1-2*3")) -5.0000)})