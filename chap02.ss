;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The definition of lanbda-calculus expression
; Lc-exp ::= Identifier
;        ::= (lambda (Identifier) Lc-exp)
;        ::= (Lc-exp Lc-exp)

;The interface for lambda-calculus expression
;Constructors
;var-exp    : Var -> Lc-exp
(define var-exp
  (lambda (var)
    (list var)))

;lambda-exp : Var * Lc-exp -> Lc-exp
(define lambda-exp
  (lambda (bound-var body)
    (list 'lambda (list bound-var) body)))
    
;app-exp    : Lc-exp * Lc-exp ->Lc-exp
(define app-exp
  (lambda (rator rand)
    (list rator rand)))

;Predicates
;var-exp?    : Lc-exp -> bool
(define var-exp? symbol?)

;lambda-exp? : Lc-exp -> bool
(define lambda-exp?
  (lambda (exp)
    (
;app-exp?    : Lc-exp -> bool

;Extractors
;var-exp->var          : Lc-exp -> Var
;lambda-exp->bound-var : Lc-exp -> var
;lambda-exp->body      : Lc-exp -> Lc-exp
;app-exp->operator     : Lc-exp -> Lc-exp
;app-exp->operand      : Lc-exp -> Lc-exp

;A version of occurs-free? that depends only on the interface
;occurs-free? : Sym * LcExp -> Bool
(define occurs-free?
  (lambda (search-var exp)
    (cond
      ((var-exp? exp) (eqv? search-var (var-exp->var exp)))
      ((lambda-exp? exp)
       (and
        (not (eqv? search-var (lambda-exp->bound-var exp)))
        (occurs-free? search-var (lambda-exp->body exp))))
      (else
       (or
        (occurs-free? search-var (app-exp->operator exp))
        (occurs-free? search-var (app-exp->operand exp)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Designing an interface for a recursive data type
;1、Include one constructor for each kind of data in the data type.
;2、Include one predicate for each kind of data in the data type.
;3、Include one extractor for each piece of data passed to a constructor of the data type.


