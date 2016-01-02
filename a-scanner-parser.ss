(load "a-scanner-spec.ss")
(load "a-parser-spec.ss")

(define scan&parse
	(sllgen:make-string-parser a-scaner-spec a-grammar-spec))
	
(scan&parse "{x := foo; while x do x := (x - bar)}")