#lang racket

;;
;; author Tooster
;; license: https://github.com/T3sT3ro/MP/blob/master/LICENSE

(define (make-state mem ptr flag register) (list mem ptr flag register))
(define (mem-gen n) (build-list n (lambda (x) 0)))
(define (get-mem state) (first state))
(define (get-ptr state) (second state))
(define (get-flag state) (third state)) ;; 1 = if(false) mode 0 = normal execution -1 = jumpto testwhile (backtrace)
(define (get-register state) (fourth state))
(define (mem-write mem ix val) (if [= ix 0] (list* val (cdr mem)) (list* (car mem) (mem-write (cdr mem) (sub1 ix) val))))
(define (mem-read mem ix) (if [= ix 0] (car mem) (mem-read (cdr mem) (sub1 ix))))
(define (opchar op) (cond [(eq? op '<) "<"] [(eq? op '>) ">"] [(eq? op '+) "+"] [(eq? op '-) "-"]
                          [(eq? op 'r) "."] [(eq? op 'w) ","] [(eq? op 'if) "["] [(eq? op 'endif) "]"]
                          [else "?"]))

(define (printstate state)
  (printf " { P:~a F:~a R:~a } \tmemory:[~a]\n" (get-ptr state) (get-flag state) (get-register state) (get-mem state)))
(define (op-primitive? op) (or (eq? op '>) (eq? op '<) (eq? op '+) (eq? op '-) (eq? op 'r) (eq? op 'w)))

(define (getop prog iptr)
    (if [or (>= iptr (string-length prog))
            (< iptr 0)]
        null
        (let ([instr (substring prog iptr (add1 iptr))])
             (cond [(string=? instr ">") '>]
                   [(string=? instr "<") '<]
                   [(string=? instr "+") '+]
                   [(string=? instr "-") '-]
                   [(string=? instr ".") 'r]
                   [(string=? instr ",") 'w]
                   [(string=? instr "[") 'if]
                   [(string=? instr "]") 'endif]
                   [else 'comment]))))

(define (nextstate state op)
  (let* ([mem (get-mem state)]
         [ptr (get-ptr state)]
         [F (get-flag state)]
         [R (get-register state)]
         [val (mem-read mem ptr)])
        (cond [(and (op-primitive? op) (not (=(get-flag state) 0))) {cons state F}] ;; primitive and flag != 0 - return flag as shift for instr ptr
              [(eq? op '+) {cons (make-state (mem-write mem ptr (add1 val)) ptr F R) 1}]
              [(eq? op '-) {cons (make-state (mem-write mem ptr (sub1 val)) ptr F R) 1}]
              [(eq? op '<) {cons (make-state mem (sub1 ptr) F R) 1}]
              [(eq? op '>) {cons (make-state mem (add1 ptr) F R) 1}]
              [(eq? op 'r) {display (integer->char (mem-read mem ptr))}{cons (make-state mem ptr F R) 1}]
              [(eq? op 'w) {let* ([c (read-char)]
                                  [c (if (eof-object? c) 0 (char->integer c))])
                             (cons (make-state (mem-write mem ptr c) ptr F R) 1)}]
              [(eq? op 'if)
                          {cond [(= F 0) (if [= val 0]
                                             (cons (make-state mem ptr 1 (add1 R)) 1) ;; skip mode - count parenthases
                                             (cons state 1 ))] ;; eval mode - next character
                                [(= F 1) (cons (make-state mem ptr F (add1 R)) 1)]
                                [(= F -1) (if [= (add1 R) 0]
                                              (cons (make-state mem ptr 0 (add1 R)) 0) ;; read same character without backtrace flag and don't move instruction ptr
                                              (cons (make-state mem ptr F (add1 R)) F))]}] ;; read next character in backtrace mode
              [(eq? op 'endif)
                             {cond [(= F 0) (cons (make-state mem ptr -1 (sub1 R)) -1)] ;; set backtrace flag, fix prefix flag to -1 and move instruction ptr 1 back
                                   [(= F 1) (if [= 0 (sub1 R)]
                                                (cons (make-state mem ptr 0 (sub1 R)) 1) ;; reset skip mode to normal mode
                                                (cons (make-state mem ptr F (sub1 R)) F))] ;; fix prefix sum and move further
                                   [(= F -1) (cons (make-state mem ptr F (sub1 R)) F)] ;; last one shouldn't trigger with with proper code 
                                   }]
              [else {cons state (if (>= F 0) 1 -1)}])))

(define (brainfuck-run prog state opptr verbose)  
  (if [not (string? prog)]
      (error "Program is not a string")
      (let ([op (getop prog opptr)])
        (cond [(null? op) state]
              [(or (>= (get-ptr state) (length (get-mem state))) (< (get-ptr state) 0)) (error "Index array out of bounds.")]
              [else (let* ([nxt (nextstate state op)]
                           [opshift (cdr nxt)]
                           [nxtst (car nxt)])
                      (if verbose (printf "[~a] \tNEXT:~a\t" opptr (opchar op)) null)
                      (if verbose (printstate state) null)
                      (brainfuck-run prog nxtst (+ opptr opshift) verbose))]))))

(define ($bf prog memory verbose) (brainfuck-run prog (make-state (mem-gen memory) 0 0 0) 0 verbose))
