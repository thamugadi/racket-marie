#lang racket
(define-syntax define-instruction
  (syntax-rules () ((_ instr opcode) (define (instr arg)
                       (bitwise-ior opcode (bitwise-and #xfff (bitwise-ior opcode arg)))))))

(define-syntax (define-instructions stx)
  (syntax-case stx ()
    ((define-instructions (lit ...) opcodes)
     (with-syntax (((index ...) (for/list ((i (length (syntax->list (syntax (lit ...)))))) i)))
       (syntax (begin
           (define-instruction lit (list-ref opcodes index))
           ...))))))

(define-syntax (define-register stx)
 (syntax-case stx ()
  ((define-register reg write index)
  (syntax (begin (define (reg m) (list-ref m index))
                 (define (write m x) (append (take m index) (list x) (drop m (+ index 1)))))))))

(define-instructions (load store add sub input output halt skipcond jump)
  (map (lambda (x) (* x #x1000)) (cdr (range 10))))

(define-register ac write-ac 0)
(define-register ir write-ir 1)
(define-register pc write-pc 2)

(define (op-instr m) (bitwise-and #xf000 (ir m))) (define (op-arg m) (bitwise-and #xfff (ir m))) 

(define (memory m) (cadddr m))
(define (access-mem m n) (car (drop (car (drop m 3)) n)))
(define (write-mem m n x) (append (take m 3) (list (append (take (memory m) n) (list x) (cdr (drop (memory m) n))))))

(define (fetch m) (write-pc (write-ir m (access-mem m (pc m))) (+ (pc m) 1)))
(define (cycle m) (cycle (execute (fetch m))))
(define (execute m)
  (match (op-instr m)
    ((== #x1000) (write-ac  m  (access-mem m (op-arg m))))
     ((== #x2000) (write-mem m  (op-arg m) (ac m)))
     ((== #x3000) (write-ac  m  (+ (ac m) (access-mem m (op-arg m)))))
     ((== #x4000) (write-ac  m  (- (ac m) (access-mem m (op-arg m)))))
     ((== #x5000) (write-ac  m  (read)))
     ((== #x6000) (print (ac m)))
     ((== #x7000) (exit))
     ((== #x8000) (if (= (ac m) 0) (write-pc m (+ (pc m) 1)) m))
     ((== #x9000) (write-pc m (op-arg m)))))

(define (init-machine initmem zerosize) (list 0 0 0 (append initmem (map (lambda (x) (* x 0)) (range zerosize)))))
