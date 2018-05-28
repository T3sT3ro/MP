#lang racket

;; sygnatura: grafy
(define-signature graph^
  ((contracted
    [graph        (-> list? (listof edge?) graph?)]
    [graph?       (-> any/c boolean?)]
    [graph-nodes  (-> graph? list?)]
    [graph-edges  (-> graph? (listof edge?))]
    [edge         (-> any/c any/c edge?)]
    [edge?        (-> any/c boolean?)]
    [edge-start   (-> edge? any/c)]
    [edge-end     (-> edge? any/c)]
    [has-node?    (-> graph? any/c boolean?)]
    [outnodes     (-> graph? any/c list?)]
    [remove-node  (-> graph? any/c graph?)]
    )))

;; prosta implementacja grafów
(define-unit simple-graph@
  (import)
  (export graph^)

  (define (graph? g)
    (and (list? g)
         (eq? (length g) 3)
         (eq? (car g) 'graph)))

  (define (edge? e)
    (and (list? e)
         (eq? (length e) 3)
         (eq? (car e) 'edge)))

  (define (graph-nodes g) (cadr g))

  (define (graph-edges g) (caddr g))

  (define (graph n e) (list 'graph n e))

  (define (edge n1 n2) (list 'edge n1 n2))

  (define (edge-start e) (cadr e))

  (define (edge-end e) (caddr e))

  (define (has-node? g n) (not (not (member n (graph-nodes g)))))
  
  (define (outnodes g n)
    (filter-map
     (lambda (e)
       (and (eq? (edge-start e) n)
            (edge-end e)))
     (graph-edges g)))

  (define (remove-node g n)
    (graph
     (remove n (graph-nodes g))
     (filter
      (lambda (e)
        (not (eq? (edge-start e) n)))
      (graph-edges g)))))

;; BUG: zła kolejność kontraktów i definicji powodowała enigmatyczne błędy...
;; sygnatura dla struktury danych
(define-signature bag^
  ((contracted
    [bag?       (-> any/c boolean?)]
    [bag-empty? (-> bag? boolean?)]
    [empty-bag  (and/c bag? bag-empty?)]
    [bag-insert (-> bag? any/c (and/c bag? (not/c bag-empty?)))]
    [bag-peek   (-> (and/c bag? (not/c bag-empty?)) any/c)]
    [bag-remove (-> (and/c bag? (not/c bag-empty?)) bag?)])))

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)

  (define (bag? s) (list? s))
  (define (bag-empty? s) (eq? s '()))
  (define empty-bag '())
  (define (bag-insert s v) (cons v s))
  (define (bag-peek s) (car s))
  (define (bag-remove s) (cdr s))
)

;; struktura danych - kolejka FIFO
;; do zaimplementowania przez studentów
(define-unit bag-fifo@
  (import)
  (export bag^)

  (define ixs second)
  (define oxs third)
  (define (bag? b) (and (list? b)
                        (eq? (length b) 3)
                        (eq? (car b) 'bag-fifo)))
  (define (bag-empty? b) (and (null? (ixs b))
                              (null? (oxs b))))
  (define empty-bag (list 'bag-fifo '() '()))

;; w poleceniu jest błąd - powinniśmy usuwać z początku listy wyjściowej jeśli jest to odwrócona wejściowa
  (define (bag-fifo i o) (if (null? o)
                             (list 'bag-fifo '() (reverse i))
                             (list 'bag-fifo i o)))
  (define (bag-insert b v) (bag-fifo (cons v (ixs b)) (oxs b)))
  (define (bag-peek b) (car (oxs b)))
  (define (bag-remove b) (bag-fifo (ixs b) (cdr (oxs b))))
)

;; sygnatura dla przeszukiwania grafu
(define-signature graph-search^
  (search))

;; otwarcie komponentu stosu
;(define-values/invoke-unit/infer bag-stack@)
;; opcja 2: otwarcie komponentu kolejki
 (define-values/invoke-unit/infer bag-fifo@)

;; implementacja przeszukiwania grafu
;; uzależniona od implementacji grafu i struktury danych
(define-unit/contract graph-search@
  (import bag^ graph^)
  (export (graph-search^
           [search (-> graph? any/c (listof any/c))]))
  (define (search g n)
    (define (it g b l)
      (cond
        [(bag-empty? b) (reverse l)]
        [(has-node? g (bag-peek b))
         (it (remove-node g (bag-peek b))
             (foldl
              (lambda (n1 b1) (bag-insert b1 n1))
              (bag-remove b)
              (outnodes g (bag-peek b)))
             (cons (bag-peek b) l))]
        [else (it g (bag-remove b) l)]))
    (it g (bag-insert empty-bag n) '()))
  )

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; graf testowy
(define test-graph
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))


(define (random-graph-gen n)
  (define (random-graph-add-edges c g)
    (if (= c 0)
        g
        (random-graph-add-edges (sub1 c) (graph (graph-nodes g) (cons (edge (random n) (random n)) (graph-edges g))))))
  (random-graph-add-edges (random (* n n)) (graph (build-list n identity) '())))

(define random-graph
  (random-graph-gen 12))

(define test-graph-2
  '(graph
  (0 1 2 3 4)
  ((edge 3 2)
   (edge 1 3)
   (edge 2 0)
   (edge 4 4)
   (edge 3 1)
   (edge 1 1)
   (edge 3 3)
   (edge 4 4)
   (edge 0 1)
   (edge 4 2)
   (edge 1 3))))


;; testy w Quickchecku
(require quickcheck)

;; test przykładowy: jeśli do pustej struktury dodamy element
;; i od razu go usuniemy, wynikowa struktura jest pusta
(quickcheck
 (property ([s arbitrary-symbol])
           (bag-empty? (bag-remove (bag-insert empty-bag s)))
           ))

;; sprawdza czy pusty stos jest na pewno pusty ........
(quickcheck
 (property () (bag-empty? empty-bag)))

(define (list-insert b xs) (if (null? xs) b (list-insert (bag-insert b (car xs)) (cdr xs))))
(define (list-remove b xs) (if (bag-empty? b) xs (list-remove (bag-remove b) (cons (bag-peek b) xs))))

;; sprawdza element na szczycie dla implementacji na kolejce
(quickcheck
 (property ([s arbitrary-symbol]
            [xs (arbitrary-list arbitrary-symbol)])
           (eq? s (bag-peek (list-insert (bag-insert empty-bag s) xs)))))

;; sprawdza element na szczycie dla implementacji na stosie

#|
(quickcheck
 (property ([s arbitrary-symbol]
            [xs (arbitrary-list arbitrary-symbol)])
           (eq? s (bag-peek (bag-insert (list-insert empty-bag xs) s)))))
|#

;; sprawdza kolejność implementacji na stacku
#|
(quickcheck (property ([xs (arbitrary-list arbitrary-symbol)])
                        (equal? xs (list-remove (list-insert empty-bag xs) '()))))
|#

;; sprawdza kolejność implementacji na kolejce
(quickcheck (property ([xs (arbitrary-list arbitrary-symbol)])
                        (equal? (reverse xs) (list-remove (list-insert empty-bag xs) '()))))

;; otwarcie komponentu przeszukiwania
(define-values/invoke-unit/infer graph-search@)

;; uruchomienie przeszukiwania na przykładowym grafie
(search test-graph 1)
(search random-graph 1)
(search test-graph-2 3)
