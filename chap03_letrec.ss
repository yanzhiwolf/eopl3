#lang eopl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LET scanner specification
(define scanner-spec
  '((white-sp (whitespace) skip)
    (commet ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number ((arbno "-") digit (arbno digit)) number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LET parser specification
(define parser-spec
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("proc" "(" identifier ")" expression) proc-exp)
    (expression ("(" expression expression ")") call-exp)))


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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;environment
;empty-env: () -> Env
(define empty-env
  (lambda () '()))


;extend-env: Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;extend-env-list: listOf(vars) * listOf(vals) * Env -> Env
(define extend-env-list
  (lambda (vars vals env)
    (if (null? vars)
        env
        (let ((var (car vars))
              (val (car vals)))
          (extend-env-list
           (cdr vars) (cdr vals) (extend-env var val env))))))

(define extend-env-rec
  (lambda (p-name b-var body env)
    (begin (display env) (newline)
           (let ((envrec (cons (cons p-name (procedure b-var body env)) env)))
             (begin (display envrec) (newline)
                    envrec)))))

;apply-env: Var * Env -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env) (report-no-binding-found search-var))
      ((eqv? (caar env) search-var)
       (let ((saved-var (cdar env)))
         (if (procedure? saved-var)
             (proc-val saved-var)
             saved-var)))
      (else
       (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (begin
      (display "no binding for ")
      (display search-var)
      (newline))))

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
  (lambda (var exp env)
    (lambda (val)
      (value-of exp (extend-env var val env)))))

;Proc * ExpVal -> ExpVal
(define apply-procedure
  (lambda (proc1 val)
    (proc1 val)))



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
                              [(null? conds) (eopl:printf 'cond)]
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

      ;(expression ("letrec" identifier "(" identifier ")" "=" expression "in" expression) letrec-exp)
      (letrec-exp (p-name b-var p-body let-body)
                  (begin (display p-name) (newline)
                         (display b-var) (newline)
                         (display p-body) (newline)
                         (value-of let-body (extend-env-rec p-name b-var p-body env))))
      
      ;(expression ("proc" "(" identifier ")" expression) proc-exp)
      (proc-exp (var body)
                (proc-val (procedure var body env)))

      ;(expression (expression expression) apply-exp)
      (call-exp (rator rand)
                (let ((proc1 (expval->proc (value-of rator env)))
                      (arg (value-of rand env)))
                  (apply-procedure proc1 arg)))
      
      )))



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
(run "let f = proc(x) proc(y) -(x, -(0, y)) in ((f 1) 2)")

(run "let makemult = proc(maker)
                       proc(x)
                         if zero?(x)
                         then 0
                         else -(((maker maker) -(x, 1)), -(0,4))
      in let times4 = proc(x) ((makemult makemult) x)
         in (times4 3)")


(run "letrec double(x) = if zero?(x)
                         then 0
                         else -((double -(x, 1)), -2)
      in (double 6)")