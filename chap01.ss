#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; chapter 01

;;;Exercise 1.15
;duple: n × x -> Listof(n)
;usage: (duple n 2) = (n n)
(define duple
  (lambda (n x)
    (if (= x 0)
        '()
	(cons
         n (duple n (- x 1))))))
		   
;(duple 'a 4)
;(duple '(1 2) 2)


;;;Exercise 1.16
;invert: lst -> Listof(2-lists reversed)
;usage: (invert '((a 1)(a 2))) = '((1 a) (2 a))
(define invert
  (lambda (lst)
    (if (null? lst)
        '()
        (cons
         (cons (cadar lst) (caar lst))
         (invert (cdr lst))))))

;(invert '((a b) (c d)))


;;;Exercise 1.17
;down: lst -> Listof(wraps paraentheses around each top-level of lst)
;usage: (down '(1 2 3)) = '((1) (2) (3))
(define down
  (lambda (lst)
    (if
     (null? lst)
     '()
     (cons (cons (car lst) '())
             (down (cdr lst))))))

;(down '())
;(down '(1 2 3))
;(down '((a) (b c) ((e))))


;;;Exercise 1.18
;swapper: s1 × s2 × slist -> slist with swap s1 and s2
;usage: (swapper 'a 'b '((a b) (c a) (d b))) = '((b a) (c b) (d a))
(define swapper
  (lambda (s1 s2 slist)
    (cond
      ((null? slist)
       '())
      ((list? (car slist))
       (cons (swapper s1 s2 (car slist))
             (swapper s1 s2 (cdr slist))))
      ((eqv? s1 (car slist))
       (cons s2 (swapper s1 s2 (cdr slist))))
      ((eqv? s2 (car slist))
       (cons s1 (swapper s1 s2 (cdr slist))))
      (else
       (cons (car slist) (swapper s1 s2 (cdr slist)))))))

;(swapper 'a 'b '(a b c d))
;(swapper 'a 'b '((a b) (c a) (d b)))
    


;;;Exercise 1.19
;list-set: lst × n × x -> lst with n-th element replaced by x
;usage: (list-set '(a b c d) 2 '(a b c)) = '(a b (a b c) d)
(define list-set
  (lambda (lst n x)
    (if
     (= n 0)
     (cons x (cdr lst))
     (cons (car lst)
           (list-set (cdr lst) (- n 1) x)))))

;(list-set '(a b c d) 2 '(a b c))
;(list-ref (list-set '(a b c d) 3 '(1 5 10)) 3)




;;;Exercise 1.20
;count-occurrences: s × slist -> number
;usage: (count-occurrences 'x '((f x) y (((x z) x))))
(define count-occurrences
  (lambda (s slist)
    (cond
     ((null? slist)
      0)
     ((list? (car slist))
      (+ (count-occurrences s (car slist))
         (count-occurrences s (cdr slist))))
     ((eqv? s (car slist))
      (+ 1 (count-occurrences s (cdr slist))))
     (else
      (+ (count-occurrences s (cdr slist)))))))

;(count-occurrences 'x '((f x) y (((x z) x))))


;;;Exercise 1.21
;product: sos1 × sos2 -> listOf(2-list)
;usage: (product '(1 2) '(a b)) = '((1 a) (1 b) (2 a) (2 b))
(define product-helper
  (lambda (s sos lop)
    (if (null? sos) lop
        (product-helper s
                        (cdr sos)
                        (cons (list s (car sos)) lop)))))

(define product
  (lambda (sos1 sos2)
    (if (or (null? sos1) (null? sos2))
        '()
        (product-helper (car sos1)
                        sos2 
                        (product (cdr sos1) sos2)))))

;(product-helper '1 '(a b c) '())
;(product '(1 2 3 4) '(a b c))
;(product '(1) '(a b c))



;;;Exercise 1.22
;filter-in: pred × lst -> listOf(satisfy the pred elements)
;usage: (filter-in number? '(a 2 (1 3) b 7)) = '(2 7)
(define filter-in
  (lambda (pred lst)
    (cond
     ((null? lst) '())
     ((pred (car lst))
        (cons (car lst)
              (filter-in pred (cdr lst))))
     (else (filter-in pred (cdr lst))))))

;(filter-in number? '(a 2 6 (1 3) b 7))



;;;Exercise 1.23
;list-index: pred × lst -> 0-based position of the first element of lst that satisfies the pred
;usage: (list-index number? '(a 2 (1 3) b 4)) = 1
(define list-index-helper
  (lambda (pred lst index)
    (cond
      ((null? lst) #f)
      ((pred (car lst)) index)
      (else (list-index-helper pred
                               (cdr lst)
                               (+ index 1))))))

(define list-index
  (lambda (pred lst)
    (list-index-helper pred lst 0)))

;(list-index number? '(a 2 (1 3) b 4))
;(list-index symbol? '((b c) 17 foo))
;(list-index symbol? '(1 2 (a b) 3))




;;;Exercise 1.24
;every?: pred × lst -> bool
;usage: (every? number? '(a b c d e f)) = #f
(define every?
  (lambda (pred lst)
    (cond
      ((null? lst) #t)
      (else
       (and (pred (car lst))
            (every? pred (cdr lst)))))))

;(every? number? '(a b c 1 e f))
;(every? number? '(1 2 3 5 4))



;1.25
;exists? pred * lst -> #t/#f
(define exists?
  (lambda (pred lst)
    (cond
      ((null? lst) #f)
      (else
       (or (pred (car lst))
           (exists? pred (cdr lst)))))))

;(exists? number? '(a b c 3 e))
;(exists? number? '(a b c d e))



;1.26
;up: lst -> lst
(define up-helper
  (lambda (lst r)
    (if (null? lst) r
        (cons (car lst)
              (up-helper (cdr lst) r)))))

(define up
  (lambda (lst)
    (cond
      ((null? lst) '())
      ((list? (car lst))
       (up-helper (car lst) (up (cdr lst))))
      (else
       (cons (car lst) (up (cdr lst)))))))

;(up '(0 (1 2) (3 4) 5 6))



;1.27
;flatten: slist -> slist (removes all the inner parentheses from its argument)
;usage: (flatten '((a) () (b ()) () (c))) = '(a b c)
(define flatten-helper
  (lambda (slist r)
    (cond
      ((null? slist) r)
      ((list? slist)
       (flatten-helper (car slist)
                       (flatten-helper (cdr slist) r)))
      (else (cons slist r)))))

(define flatten
  (lambda (slist)
    (if (null? slist) '()
        (flatten-helper (car slist)
                              (flatten (cdr slist))))))

;(flatten '((a) b))
;(flatten '((a) () (b ()) () (c)))
;(flatten '((a b) c (((d)) e)))
;(flatten '(a b (() (c))))



;1.28
;merge: loi1 × loi2 -> loi merged
;usage: (merge '(35 62 81 90 91) '(3 83 85 90)) = '(3 35 62 81 83 85 90 90 91)
(define merge
  (lambda (loi1 loi2)
    (cond
      [(null? loi1) loi2]
      [(null? loi2) loi1]
      [(<= (car loi1) (car loi2))
       (cons (car loi1)
             (merge (cdr loi1) loi2))]
      [else
       (cons (car loi2) (merge loi1 (cdr loi2)))])))
       
;(merge '(35 62 81 90 91) '(3 83 85 90))




;1.29
;sort: loi -> loi sorted
;usage: (sort '(8 2 5 2 3)) = '(2 2 3 5 8)
(define sort-insert
  (lambda (i loi)
    (cond
      [(null? loi) (cons i loi)]
      [(<= i (car loi)) (cons i loi)]
      [else
       (cons (car loi)
             (sort-insert i (cdr loi)))])))

(define sort
  (lambda (loi)
    (if (null? loi) '()
        (sort-insert (car loi)
                     (sort (cdr loi))))))

;(sort '(8 2 5 2 3))
;(merge (sort '(2 5 1 8 3 2 4)) (sort '(0 9 3 1 6 4 7)))



;1.30
;sort/predicate: pred × loi -> loi
;usage: (sort/predicate < ’(8 2 5 2 3))
(define sort/predicate-insert
  (lambda (pred i loi)
    (cond
      [(null? loi) (cons i loi)]
      [(pred i (car loi)) (cons i loi)]
      [else
       (cons (car loi)
             (sort/predicate-insert pred i (cdr loi)))])))

(define sort/predicate
  (lambda (pred loi)
    (if (null? loi) '()
        (sort/predicate-insert pred
                     (car loi)
                     (sort/predicate pred (cdr loi))))))

;(sort/predicate < '(8 2 5 2 3))
;(sort/predicate > '(8 2 5 2 3))


;Definition 1.1.7(binary tree)
;Bintree ::= Int | (Symbol Bintree Bintree)

;1.31
;procedures for calculating on a bintree
(define leaf
  (lambda (c)
    (list c '() '())))

(define interior-node
  (lambda (c lson rson)
    (list c lson rson)))

(define leaf?
  (lambda (node)
    (and (null? (cadr node))
         (null? (caddr node)))))

(define lson
  (lambda (node)
    (cadr node)))

(define rson
  (lambda (node)
    (caddr node)))

(define contents-of
  (lambda (node)
    (car node)))


(define tree-test-int
  (interior-node '1
                 (interior-node '2 (leaf '3) (leaf '4))
                 (interior-node '5 (leaf '6) (leaf '7))))
;tree-test-int ; => '(1 (2 (3 () ()) (4 () ())) (5 (6 () ()) (7 () ())))


;1.32
;double-tree: tree -> tree with all leaf doubled
(define double-tree
  (lambda (tree)
    (if (leaf? tree)
        (cons (* 2 (contents-of tree)) '())
        (list (contents-of tree)
              (double-tree (lson tree))
              (double-tree (rson tree))))))

;(double-tree tree-test-int) ;=> '(1 (2 (6) (8)) (5 (12) (14)))



;1.33
;make-leaves-with-red-depth: tree -> other tree
(define make-leaves-with-red-depth-helper
  (lambda (tree red-depth)
    (cond
      ((leaf? tree) (leaf red-depth))
      ((eqv? (contents-of tree) 'red)
        (list (contents-of tree)
              (make-leaves-with-red-depth-helper (lson tree) (+ red-depth 1))
              (make-leaves-with-red-depth-helper (rson tree) (+ red-depth 1))))
      (else
       (list (contents-of tree)
              (make-leaves-with-red-depth-helper (lson tree) red-depth)
              (make-leaves-with-red-depth-helper (rson tree) red-depth))))))
     
(define make-leaves-with-red-depth
  (lambda (tree)
    (make-leaves-with-red-depth-helper tree 0)))

(define tree-test-red
  (interior-node 'red
                 (interior-node 'bar (leaf '3) (leaf '4))
                 (interior-node 'red (leaf '6) (leaf '7))))
;tree-test-red
;(make-leaves-with-red-depth tree-test-red)



;1.34
;path n * bst -> listOf(search path)
;usage: (path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ())))) = (right left left)
(define path
  (lambda (n bst)
    (cond
      [(eqv? n (contents-of bst)) '()]
      [(< n (contents-of bst))
       (cons 'left (path n (lson bst)))]
      [else
       (cons 'right (path n (rson bst)))])))
       

;(path 17 '(14 (7 () (12 () ())) (26 (20 (17 () ()) ()) (31 () ()))))



;1.35
;number-leaves: bintree -> bintree
(define number-leaves-helper
  (lambda (bintree number number-bintree)
    (cond
      ((leaf? bintree) (leaf (+ number 1))))))

(define number-leaves
  (lambda (bintree)
    (number-leaves-helper bintree 0 '())))

;1.36


















































