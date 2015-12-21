
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
    (if (null? lst) '()
        (up-helper (car lst)
                   (up (cdr lst))))))

(up '((1 2) (3 4) 5 6))

          
                     
              























