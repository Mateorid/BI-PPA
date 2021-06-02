#lang racket
(require compatibility/defmacro)

(define (my-while i ub foo)
  (if (> i ub)
  (void)
  (begin
    (foo i)
    (my-while (+ i 1) ub foo))))

(define-macro (loop from to body)
  `(my-while ,from ,to (lambda(i) ,body)))

(loop 69 89 (printf "test: ~s\n" i))