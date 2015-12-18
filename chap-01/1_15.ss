;;;duple: n X x -> Listof(n)
;;;usage: (duple n 2) = (n n)

(define duple
	(lambda (n x)
		(if (= x 0)
		   '()
		   (cons n (duple n (- x 1))))))
		   
(duple 'a 4)
(duple '(1 2) 2)