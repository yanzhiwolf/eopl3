(load "a-scanner-spec.ss")
(load "a-parser-spec.ss")

(sllgen:make-define-datatypes a-scaner-spec a-grammar-spec)

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes a-scaner-spec a-grammar-spec)))

(define just-scan
  (sllgen:make-string-scanner a-scaner-spec a-grammar-spec))

(define scan&parse
	(sllgen:make-string-parser a-scaner-spec a-grammar-spec))

(scan&parse "{x := foo; while x do x := (x - bar)}")