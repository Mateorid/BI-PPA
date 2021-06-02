; helpers
(define (atom? x)
  (and (not (null? x))
       (not (pair? x))))

; ------------------------------------------------------------------------------
(define (inc n) (+ n 1))
(define (dec n) (- n 1))
(define (zero n) (= n 0))
(define (add x y)
  (if (zero x)
      y
      (add (dec x) (inc y))))

(define (mult x y)
  (mult2 x y 0))
(define (mult2 m1 m2 res)
  (if (zero m1)
      res
      (mult2 (dec m1) m2 (add res m2))))

(define (my-min x y)
  (if (> x y) y x))

(define (my-max x y)
  (if (> x y) x y))

; factorial a fibonacci normalne
(define (my-fact n)
  (if (<= n 1) 1
      (* n (my-fact (- n 1)))))

(define (my-fib n)
  (if (<= n 2) 1
      (+ (my-fib (- n 1)) (my-fib (- n 2)))))

; factorial a fibonacci koncovou rekurzi
(define (my-fact-tail n)
  (my-fact-tail-aux n 1))

(define (my-fact-tail-aux n res)
  (if (<= n 1)
      res
      (my-fact-tail-aux (- n 1) (* n res))))

(define (my-fib-tail n)
  (my-fib-tail-aux n 1 1))
(define (my-fib-tail-aux n p1 p2)
  (if (<= n 2)
      p1
      (my-fib-tail-aux (- n 1) (+ p1 p2) p1)))

(define (my-sum lst)
  (if (null? lst) 0
      (+ (car lst) (my-sum (cdr lst)))))

(define (my-sum-tail lst)
  (my-sum-tail-aux lst 0))
(define (my-sum-tail-aux lst res)
  (if (null? lst)
      res
      (my-sum-tail-aux (cdr lst) (+ (car lst) res))))

(define (my-nth lst n)
  (cond
    ((null? lst) null)
    ((= n 0)     (car lst))
    (#t          (my-nth (cdr lst) (- n 1)))))

; my-maxL is done similarly
(define (my-minL lst)
  (cond
    ((null? lst)       null)
    ((null? (cdr lst)) (car lst))
    (#t                (my-min (car lst) (my-minL (cdr lst))))))

(define (my-member lst x)
  (cond
    ((null? lst)           #f)
    ((equal? (car lst) x)  #t)
    (#t                    (my-member (cdr lst) x))))

(define (my-notmember lst x)
  (not (my-member lst x)))    ; :-)

(define (my-notmember2 lst x)
  (cond
    ((null? lst)          #t)
    ((equal? (car lst) x) #f)
    (#t                   (my-notmember2 (cdr lst) x))))

(define (my-range n)
  (my-range2 0 n))

(define (my-range2 i upper_bound)
  (if (= i upper_bound) '()
      (cons i (my-range2 (+ i 1) upper_bound))))

(define (my-append lst x)
  (if (null? lst)
      (cons x null)
      (cons (car lst) (my-append (cdr lst) x))))

(define (my-prepend lst x)
  (cons x lst))

(define (my-appendL lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1) (my-appendL (cdr lst1) lst2))))

(define (my-insert lst n e)
  (cond
    ((null? lst) (cons e null))
    ((= n 0)     (cons e lst))
    (#t          (cons (car lst) (my-insert (cdr lst) (- n 1) e)))))

(define (my-repeat n e)
  (if (= n 0)
      '()
      (cons e (my-repeat (- n 1) e))))

(define (my-reverse lst)
  (if (null? lst)
      '()
      (my-append (my-reverse (cdr lst)) (car lst))))

(define (my-delete-first lst e)
  (cond
    ((null? lst)          '())
    ((equal? (car lst) e) (cdr lst))
    (#t                   (cons (car lst) (my-delete-first (cdr lst) e)))))

(define (my-delete-all lst e)
  (cond
    ((null? lst)          '())
    ((equal? (car lst) e) (my-delete-all (cdr lst) e))
    (#t                   (cons (car lst) (my-delete-all (cdr lst) e)))))

(define (my-delete-last lst e)
  (my-reverse (my-delete-first (my-reverse lst) e)))

(define (my-insert-between-all lst e)
  (cons (car lst) (my-insert-between-all-aux (cdr lst) e)))
(define (my-insert-between-all-aux lst e)
  (if (null? lst)
      null
      (cons e (cons (car lst) (my-insert-between-all-aux (cdr lst) e)))))

(define (my-memberS lst e)
  (cond
    ((null? lst) #f)
    ((atom? (car lst)) (if (equal? (car lst) e)
                           #t
                           (my-memberS (cdr lst) e)))
    (#t                (or (my-memberS (car lst) e) (my-memberS (cdr lst) e)))))

(define (my-sumS lst)
  (cond
    ((null? lst) 0)
    ((atom? (car lst)) (+ (car lst) (my-sumS (cdr lst))))
    (#t                (+ (my-sumS (car lst)) (my-sumS (cdr lst))))))

(define (my-sumS-tail lst)
  (my-sumS-tail-aux lst 0))
(define (my-sumS-tail-aux lst acc)
  (cond
    ((null? lst) acc)
    ((atom? (car lst)) (my-sumS-tail-aux (cdr lst) (+ acc (car lst))))
    (#t                (my-sumS-tail-aux (cdr lst) (+ acc (my-sumS (car lst)))))))

(define (my-lengthS lst)
  (cond
    ((null? lst) 0)
    ((atom? (car lst)) (+ 1 (my-lengthS (cdr lst))))
    (#t                (+ (my-lengthS (car lst)) (my-lengthS (cdr lst))))))
(define (my-meanS lst)
  (/ (my-sumS lst) (my-lengthS lst)))

(define (my-flatten lst)
  (cond
    ((null? lst) null)
    ((atom? (car lst)) (cons (car lst) (my-flatten (cdr lst))))
    (#t                (my-appendL (my-flatten (car lst)) (my-flatten (cdr lst))))))

(define (my-flatten-tail lst)
  (my-flatten-tail-aux lst null))
(define (my-flatten-tail-aux lst acc)
  (cond
    ((null? lst) acc)
    ((atom? (car lst)) (my-flatten-tail-aux (cdr lst) (my-append acc (car lst))))
    (#t                (my-flatten-tail-aux (cdr lst) (my-appendL acc (my-flatten-tail (car lst)))))))

; two variants for my-split (my-split-1, my-split2)
(define (my-split-1 lst)
  (my-split-1-aux lst null null 1))
(define (my-split-1-aux lst a1 a2 put-into)
  (cond
    ((null? lst)    (cons a1 a2)) ; result in the form: 1st array: (car (my-split ...)), 2nd array: (cdr (my-split ....))
    ((= put-into 1) (my-split-1-aux (cdr lst) (cons (car lst) a1) a2 2))
    ((= put-into 2) (my-split-1-aux (cdr lst) a1 (cons (car lst) a2) 1))))

(define (my-split-2 lst)
  (my-split2-aux lst (/ (my-lengthS lst) 2)))
(define (my-split2-aux lst n)
  (if (<= n 0)
      (cons null lst)
      (let ((subresult (my-split2-aux (cdr lst) (- n 1)))) ; (car subresult) -> 1st half (we have to modify), (cdr subresult) -> 2nd half (do not modify)!
        (cons
         (cons (car lst) (car subresult)) ; modified 1st half
         (cdr subresult)))))              ; unmodified 2nd half

(define (my-merge l1 l2)
  (cond
    ((null? l1) l2)
    ((null? l2) l1)
    ((< (car l1) (car l2)) (cons (car l1) (my-merge (cdr l1) l2)))
    (#t                    (cons (car l2) (my-merge l1 (cdr l2))))))

(define (my-merge-tail l1 l2)
  (my-merge-tail-aux l1 l2 null))
(define (my-merge-tail-aux l1 l2 acc)
  (cond
    ((null? l1) (my-appendL acc l2))
    ((null? l2) (my-appendL acc l1))
    ((< (car l1) (car l2)) (my-merge-tail-aux (cdr l1) l2 (my-append acc (car l1))))
    (#t                    (my-merge-tail-aux l1 (cdr l2) (my-append acc (car l2))))))

(define (my-mergesort lst)
  (cond
    ((null? lst) null)
    ((null? (cdr lst)) lst)
    (#t     (let ((split-lst (my-split-1 lst)))
              (my-merge
               (my-mergesort (car split-lst))
               (my-mergesort (cdr split-lst)))))))

(define (pow2 n) (* n n))
(define (mod n div)
  (- n (* div (floor (/ n div)))))

(define (my-prime? n)
  (my-prime-aux 2 n))

(define (my-prime-aux divisor n)
  (cond
    ((> (pow2 divisor) n)  #t)
    ((= (mod n divisor) 0) #f)
    (#t                    (my-prime-aux (+ 1 divisor) n))))

(define (my-powerset lst)
  (cond
    ((null? lst)       null)
    ((null? (cdr lst)) (cons null (cons (cons (car lst) null) null))) ; (powerset '(A)) = '( () (A))
    (#t                (cartesian (my-powerset (cons (car lst) null)) (my-powerset (cdr lst)))))) ; (powerset '(A B...)) = (powerest '(B ... ))

(define (cartesian set1 set2)
  (if (null? set1)
      null
      (my-appendL (cartesian-helper (car set1) set2) (cartesian (cdr set1) set2))))

(define (cartesian-helper e set)
  (if (null? set)
      null
      (cons (my-appendL e (car set)) (cartesian-helper e (cdr set)))))

(define (my-fact-list-naive n)
  (my-fact-list-naive-aux 1 n))
(define (my-fact-list-naive-aux i n)
  (if (> i n)
      null
      (cons (my-fact i) (my-fact-list-naive-aux (+ 1 i) n ))))

(define (my-fact-list n)
  (my-reverse (my-fact-list-aux 2 n '(1))))
(define (my-fact-list-aux i n acc)
  (if (> i n)
      acc
      (my-fact-list-aux (+ i 1) n (cons (* i (car acc)) acc))))

(define (my-unique lst)
  (my-unique-aux lst null))
(define (my-unique-aux lst prev)
  (cond
    ((null? lst) lst)
    ((equal? prev (car lst)) (my-unique-aux (cdr lst) (car lst)))
    (#t                      (cons (car lst) (my-unique-aux (cdr lst) (car lst))))))

(define (my-rle lst)
  (my-rle-aux lst null 0))
(define (my-rle-aux lst prev cnt)
  (cond
    ; (my-rle '())
    ((and (null? prev) (null? lst)) null)
    ; end of recursion -> print last item
    ((null? lst) (cons (list cnt prev) null))
    ; if first, dont output yet, just note the first occurence
    ((and (null? prev))      (my-rle-aux (cdr lst) (car lst) 1))
    ((equal? prev (car lst)) (my-rle-aux (cdr lst) (car lst) (+ cnt 1)))
    (#t                      (cons (list cnt prev) (my-rle-aux (cdr lst) (car lst) 1)))))

(define (my-pack lst)
  (my-pack-aux (my-rle lst)))
(define (my-pack-aux lst)
  (if (null? lst)
      null
      (cons
       (my-repeat (caar lst) (cadar lst))
       (my-pack-aux (cdr lst)))))
      

; bst = '()
; bst = '(val bst-left bst-right)
(define (bst-v bst) (car bst))
(define (bst-l bst) (cadr bst))
(define (bst-r bst) (caddr bst))

(define (bst-find bst e)
  (cond
    ((null? bst) #f)
    ((equal? e (bst-v bst)) #t)
    ((< e (bst-v bst)) (bst-find (bst-l bst) e))
    ((> e (bst-v bst)) (bst-find (bst-r bst) e))))

(define (bst-construct val bst-left bst-right)
  (cons val (cons bst-left (cons bst-right null))))

(define (bst-insert bst e)
  (cond
    ((null? bst) (bst-construct e null null))
    ((equal? e (bst-v bst)) bst) ; do nothing, value already in bst
    ((< e (bst-v bst)) (bst-construct (bst-v bst) (bst-insert (bst-l bst) e) (bst-r bst)))
    ((> e (bst-v bst)) (bst-construct (bst-v bst) (bst-l bst) (bst-insert (bst-r bst) e)))))

(define (bst-preorder bst)
  (if (null? bst)
      '()
      (cons (bst-v bst)
            (my-appendL (bst-preorder (bst-l bst)) (bst-preorder (bst-r bst))))))

(define (bst-inorder bst)
  (if (null? bst)
      '()
      (my-appendL (bst-inorder (bst-l bst))
                  (cons (bst-v bst) (bst-inorder (bst-r bst))))))

(define (bst-postorder bst)
  (if (null? bst)
      '()
      (my-appendL (bst-postorder (bst-l bst))
                  (my-appendL (bst-postorder (bst-r bst))
                              (cons (bst-v bst) null)))))
                              

(define (bst-height bst)
  (if (null? bst)
      -1
      (+ 1 (my-max (bst-height (bst-l bst)) (bst-height (bst-r bst))))))

(define (bst-min bst)
  (cond
    ((null? bst)         null) ; no min
    ((null? (bst-l bst)) (bst-v bst)) ; no left child, i am the min
    (#t                  (bst-min (bst-l bst)))))

(define (mat-dim mat)
  (cons (length mat) (length (car mat))))

(define (mat-row mat i)
  (my-nth mat i))

(define (mat-col mat i)
  (map (lambda (row) (my-nth row i)) mat))

(define (mat-get mat i j)
  (my-nth (mat-row mat i) j))

(define (mat-sum mat1 mat2)
  (map (lambda (row1 row2)
         (map (lambda (e1 e2) (+ e1 e2)) row1 row2 ))
       mat1 mat2))

(define (mat-diag mat)
  (mat-diag-aux mat 0))
(define (mat-diag-aux mat i)
  (cond
    ((null? mat) null)
    ((>= i (length (car mat))) null) ; rectangular matrix
    (#t (cons (my-nth (car mat) i) (mat-diag-aux (cdr mat) (+ 1 i))))))

(define (mat-sumScalar mat x)
  (map (lambda (row) (map (lambda (e) (+ e x)) row )) mat))

(define (mat-multScalar mat x)
  (map (lambda (row) (map (lambda (e) (* e x)) row )) mat))

(define (mat-crop mat)
  (mat-crop-cols (mat-crop-rows mat)))

(define (mat-crop-rows mat)
  (my-remove-first (my-remove-last mat)))
(define (mat-crop-cols mat)
  (map (lambda (row) (my-remove-last (my-remove-first row))) mat))

(define (my-remove-first lst)
  (cdr lst))
(define (my-remove-last lst)
  (if (null? (cdr lst))
      null
      (cons (car lst) (my-remove-last (cdr lst)))))

(define (my-slice lst lb ub)
  (my-slice-aux lst lb ub 0))
(define (my-slice-aux lst lb ub i)
  (cond
    ((null? lst) null)
    ((and (>= i lb) (< i ub)) (cons (car lst) (my-slice-aux (cdr lst) lb ub (+ 1 i))))
    (#t                       (my-slice-aux (cdr lst) lb ub (+ 1 i)))))

; (my-rotate-l '(0 1 2 3 4 5 6 7 8) 3)
; (0 1 2 . 3 4 5 6 7 8)
(define (my-mod n k)
  (- n (* k (floor (/ n k)))))
(define (my-rotate-l lst n)
  (let ((len (length lst)))
    (if (= len 0)
        null
        (let ((n2 (my-mod n len)))
          (let ((left  (my-slice lst 0 n2))
                (right (my-slice lst n2 len)))
            (my-appendL right left))))))

(define (my-foldl foo init lst)
  (if (null? lst)
      init
      (my-foldl foo (foo init (car lst)) (cdr lst))))

(define (my-foldr foo init lst)
  (if (null? lst)
      init
      (foo (my-foldr foo init (cdr lst)) (car lst))))

(define (my-map foo lst)
  (my-foldr (lambda (acc x) (cons (foo x) acc)) null lst)) ; foldl reverzne

(define (my-filter foo lst)
  (my-foldr (lambda (acc x) (if (foo x) (cons x acc) acc)) null lst))

(define (my-append2 lst e)
  (my-foldr (lambda (acc n) (cons n acc)) (cons e null) lst))

; naive solution without fold{l,r}
(define (my-zip-map-naive foo lst1 lst2)
  (cond
    ((null? lst1) null)
    ((null? lst2) null)
    (#t           (cons (foo (car lst1) (car lst2)) (my-zip-map-naive foo (cdr lst1) (cdr lst2))))))

(define (my-zip lst1 lst2)
  (my-zip-map-naive (lambda (n1 n2) (cons n1 (cons n2 null))) lst1 lst2))

;(define (my-treesort lst)
;  (bst-inorder (my-foldr bst-insert null lst)))

(define (my-mean lst)
  (let ((pair (my-foldr (lambda (acc n) (list (+ n (car acc)) (+ 1 (cadr acc))))
                        (list 0 0) ; acc contains a list (sum cnt)
                        lst)))
    (let ((sum (car pair)) (cnt (cadr pair))) ; just for easier access to first and second element of list "pair"
      (if (= cnt 0)
          null
          (/ sum cnt)))))

(define (my-select-middle-element lst)
  (if (null? lst)
      null
      (my-select-middle-element-aux lst lst)))
(define (my-select-middle-element-aux p1 p2)
  (cond
    ((null? p2)        (car p1))
    ((null? (cdr p2))  (car p1))
    (#t                (my-select-middle-element-aux (cdr p1) (cddr p2)))))

(define (my-filter-gt lst n) (my-filter (lambda (x) (> x n)) lst))
(define (my-filter-lt lst n) (my-filter (lambda (x) (< x n)) lst))
(define (my-filter-eq lst n) (my-filter (lambda (x) (= x n)) lst))

(define (my-quicksort lst)
  (cond
    ((null? lst)       lst)
    ((null? (cdr lst)) lst)
    (#t                (let ((pivot (my-select-middle-element lst)))
                         (let ((left  (my-filter-lt lst pivot))
                               (right (my-filter-gt lst pivot))
                               (mid   (my-filter-eq lst pivot)))
                           (my-appendL (my-quicksort left)
                                       (my-appendL mid
                                                   (my-quicksort right))))))))

(define (my-sieve n)
  (my-sieve-aux (cddr (my-range (+ n 1))))) ; initialize with list 2...n

(define (my-sieve-aux sieve)
  (if (null? sieve)
      null
      (cons (car sieve)                                                                         ; move first element (surely a prime) into the result
            (my-sieve-aux (my-filter (lambda (e) (not (= 0 (modulo e (car sieve))))) sieve))))) ; erase all elements that are not divisible by first element of the list

(define-macro (if-null val tb fb)
  `(if (null? ,val)
       ,tb
       ,fb))


(define-macro (my-let capture-pair body)
  `(
    (lambda (,(car capture-pair)) ,body)
    ,(cadr capture-pair)
   )
)

(define-macro (debug-print body)
  (let ((tmp (gensym)))  ; in macro expansion time, generate new variable name -> check in macro expander that this line is not in the expanded code, only the lines below
     `(let ((,tmp ,body))
        (println (format "!! debugger: ~a -> ~a" ',body ,tmp))

(define-macro (my-assert-true body)
  `(unless ,body
     (error (format "Failed assertion '(assert-true ~a)'" ',body))))

(define-macro (my-assert-equals body1 body2)
  `(unless (equal? ,body1 ,body2)
     (error (format "Failed assertion '(assert-equals ~a ~a)'" ',body1 ',body2))))

(define (my-while i ub foo)
  (if (= i ub)
      (void); null
      (begin
        (foo)
        (my-while (+ i 1) ub foo))))
(define (my-while2 i ub foo)
  (if (= i ub)
      (void); null
      (begin
        (foo i)
        (my-while2 (+ i 1) ub foo))))

(define-macro (my-loop1 lb ub body)
  `(my-while  ,lb ,ub (lambda () ,body)))
(define-macro (my-loop2 lb ub body)
  `(my-while2 ,lb ,ub (lambda (n) ,body)))
(define-macro (my-loop3 lb ub varname body)
  `(my-while2 ,lb ,ub (lambda (,varname) ,body)))

(my-loop1 5 7   (println (format "loop1")))
(my-loop2 5 7   (println (format "loop2 ~a" n)))
(my-loop3 5 7 x (println (format "loop3 ~a" (* x 3))))
