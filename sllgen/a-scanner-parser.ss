(load "a-scanner-spec.ss")
(load "a-parser-spec.ss")

(define scan&parse-a
	(sllgen:make-string-parser a-scaner-spec a-grammar))
	
(scan&parse-a "{x := foo; while x do x := (x - bar)}")