
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;3.21
;Extend the language of this session to include procedures with
;multiple arguments and calls with multiple operands, as suggested
;by the grammar
;       Expression ::= proc ({Identifier}*) Expression
;       Expression ::= (Expression {Expression}*)


(load "sllgen/chez-init.ss")
(load "environment.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;scanner specification
(define scanner-spec
  '((white-sp (whitespace) skip)
    (commet ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;parser specification
(define parser-spec
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("proc" "(" (arbno identifier) ")" expression) proc-exp)
    (expression ("(" expression (arbno expression) ")") call-exp)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;parser define
(define scan-parse
  (sllgen:make-string-parser scanner-spec parser-spec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for debug
(define just-scan
  (sllgen:make-string-scanner scanner-spec parser-spec))

(define list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes scanner-spec parser-spec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;define datatypes
(sllgen:make-define-datatypes scanner-spec parser-spec)


;init-env: () -> Env
;usage: (init-env) = [i=1, v=5, x=10]
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10)
       (empty-env))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The interface of expressed value
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?))
  (proc-val (proc proc?)))

;expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (eopl:pretty-print 'num val)))))

;expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (eopl:pretty-print 'bool val)))))

;expval->proc : ExpVal -> Proc
(define expval->proc
  (lambda (val)
    (cases expval val
      (proc-val (proc) proc)
      (else (eopl:pretty-print 'num val)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;The interface of proc
;SchemeVal -> Bool
(define proc? procedure?)

;Var * Exp * Env -> Proc
(define procedure
  (lambda (vars exp env)
    (lambda (vals)
      (value-of exp (extend-env-list vars vals env)))))

;Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 vals)
    (proc1 vals)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Interpreter
;run : String -> ExpVal
(define run
  (lambda (pgm-text)
    (value-of-program (scan-parse pgm-text))))

;value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm-ast)
    (cases program pgm-ast
      (a-program (exp1) (value-of exp1 (init-env))))))

;value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      ;(expression (number) const-exp)
      (const-exp (num) (num-val num))

      ;(expression (identifier) var-exp)
      (var-exp (var) (apply-env env var))

      (sub-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val (- num1 num2)))))

      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      ;(expression ("if" boolean "then" expression "else" expression) if-exp)
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))

      ;(expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
      (cond-exp (args1 args2)
                (letrec ((cond-val
                          (lambda (conds acts)
                            (cond
                              [(null? conds) (report-expression-error 'cond)]
                              [(expval->bool (value-of (car conds) env)) (value-of (car acts) env)]
                              [else
                               (cond-val (cdr conds) (cdr acts))]))))
                  (cond-val args1 args2)))

      ;(expression ("let" identifier "=" expression "in" expression) let-exp)))
      (let-exp (vars exps body)
               (let ((vals (map (lambda (exp) (value-of exp env)) exps)))
                   (value-of body (extend-env-list vars vals env))))

      ;(expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
      (let*-exp (vars exps body)
                (letrec ((bind-let*
                          (lambda (varlst explst env-let*)
                            (if (null? varlst)
                                env-let*
                                (let ((var (car varlst))
                                      (val (value-of (car explst) env-let*)))
                                  (bind-let* (cdr varlst) (cdr explst) (extend-env var val env-let*)))))))
                  (value-of body (bind-let* vars exps env))))

      ;(expression ("proc" "(" identifier ")" expression) proc-exp)
      (proc-exp (vars body)
                (proc-val (procedure vars body env)))

      ;(expression (expression expression) apply-exp)
      (call-exp (rator rands)
                (let ((proc1 (expval->proc (value-of rator env)))
                      (args (map (lambda (rand)
                                   (value-of rand env)) rands)))
                  (apply-procedure proc1 args))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test
(run "5")
(run "i")
(run "-(i, -(v, x))")
(run "if zero?(0) then 1 else 2")
(run "let y = -(i, -(v,x)) in y")
(run "let x=30 in let x = -(x,1) y = -(x,2) in -(x, y)")   ;=1
(run "let x=30 in let* x = -(x,1) y = -(x,2) in -(x,y)")  ;=2
(run "let f = proc (x) -(x,11) in (f (f 77))")
(run "let f = proc(x y) -(x, -(0,y)) in (f 2 3)")
