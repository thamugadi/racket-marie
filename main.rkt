#lang racket

(define-syntax define-instruction
  (syntax-rules () ((_ instr opcode)
                    (define-syntax instr
                      (syntax-rules () ((_ arg)
                       (bitwise-ior opcode (bitwise-and #xfff (bitwise-ior opcode arg)))))))))
                       
(define-instruction load  #x1000) (define-instruction store #x2000) (define-instruction add  #x3000)
(define-instruction sub   #x4000) (define-instruction input #x5000) (define-instruction output  #x6000)
(define-instruction halt  #x7000) (define-instruction skipcond #x8000) (define-instruction jump #x9000)

(define (ac m) (car m)) (define (ir m) (cadr m)) (define (pc m) (caddr m))
(define (op-instr m) (bitwise-and #xf000 (ir m))) (define (op-arg m) (bitwise-and #xfff (ir m))) 

(define (write-ac m x) (append (list x) (cdr m))) 
(define (write-ir m x) (append (take m 1) (list x) (cddr m))) 
(define (write-pc m x) (append (take m 2) (list x) (cdddr m)))

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
