#lang racket
(require compatibility/defmacro)
;Add
(define (my-add x y)
  (+ x y))
;Max
(define (my-max x y)
  (if (> x y) x y))
;;Fib
(define (my-fib n)
  (if(= n 0)
     1
     (if (= n 1)
         1
         (+ (my-fib (- n 1))(my-fib(- n 2))))))
;;Factorial
(define (my-fac n)
  (if( = n 0)
     1
     (* (my-fac (- n 1)) n)))
;(my-nth '(10 11 12 13) 2) --> 12
(define (my-nth lst n)
  (if (= n 0)
      (car lst)
      (my-nth (cdr lst) (- n 1))))
;maximum ze seznamu
(define (my-maxL lst)
  (if (null? lst)
      null;dummy exception
  (if (null? (cdr lst))
      (car lst)
      (my-max (car lst) (my-maxL (cdr lst))))))
;insert e at n-th pos
(define (my-insert lst n e)
  (if (= n 0)
      (cons e lst)
      (cons (car lst)(my-insert ( cdr lst) (- n 1) e))))
;;;Insert e in every space
(define (my-insert-all lst e)
  (if (null? lst)
      (cons e null)
      (cons e (cons (car lst)(my-insert-all (cdr lst) e)))))
;;;While loop
(define (my-while i ub foo)
  (if (> i ub)
  (void)
  (begin
    (foo i)
    (my-while (+ i 1) ub foo))))

(define-macro (loop from to body)
  `(my-while ,from ,to (lambda(i) ,body)))

(loop 69 89 (printf "test: ~s\n" i))
;;;Sum neighbours
(define(sumNeighbours lst)
  (if(null? lst)
     null
     (if (null? (cdr lst))
         (cons (car lst) null)
         (cons (+(car lst) (cadr lst))(sumNeighbours (cdr (cdr lst)))))))
;;;Sum all with using above
(define(reduce lst)
  (if(null? lst) 0
     (if(null? (cdr lst)) (car lst)
        (reduce (sumNeighbours lst)))))
;;;foldR
(define (myFoldr foo init lst)
  (if (null? lst) init
      (foo (myFoldr foo init (cdr lst)) (car lst))))
;;;foldL
(define (myFoldl foo init lst)
  (if (null? lst) init
      (myFoldl foo (foo init (car lst)) (cdr lst))))
;;;coun elements
(define (timesIn e lst)
  (if (null? lst) 0
      (myFoldr = e lst)))
;;;Lenght
(define (myLen lst)
  (if (null? lst)
      0
      (+ 1 (myLen (cdr lst)))))
;;;Append lists
(define (myAppendL lst1 lst2)
  (if (null? lst1) lst2
      (cons (car lst1) (myAppendL (cdr lst1) lst2))))
;;;Append atom
(define (myAppend lst n)
  (if (null? lst) (cons n null)
      (cons (car lst) (myAppend (cdr lst) n))))
;;;Sum list
(define (sumList lst)
  (if(null? lst) 0
     (+ (car lst) (sumList (cdr lst)))))
;;;Reverse list
(define(myRev lst)
  (if (null? lst) null
      (myAppend (myRev (cdr lst)) (car lst))))
;;;Map & lambda
(define (incAll lst)
  (map (lambda (x) (+ x 1)) lst))
;;;Atom check
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))
;;;;;;;;;;;;
(define(lessThenFive num)
  (if(< num 5) #t #f))







  