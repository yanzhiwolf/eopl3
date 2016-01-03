(load "sllgen/chez-init.ss")
(load "environment.ss")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LET scanner specification
(define let-scanner-spec
  '((white-sp (whitespace) skip)
    (commet ("%" (arbno (not #\newline))) skip)
    (identifier (letter (arbno (or letter digit))) symbol)
    (number (digit (arbno digit)) number)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LET parser specification
(define let-parser-spec
  '((program (expression) a-program)
    (expression (number) const-exp)
    (expression (identifier) var-exp)
    (expression ("-" "(" expression "," expression ")") diff-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("let" identifier "=" expression "in" expression) let-exp)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;parser define
(define let-scan-parse
  (sllgen:make-string-parser let-scanner-spec let-parser-spec))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;for debug
(define let-just-scan
  (sllgen:make-string-scanner let-scanner-spec let-parser-spec))

(define let-list-the-datatypes
  (lambda ()
    (sllgen:list-define-datatypes let-scanner-spec let-parser-spec)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;LET define datatypes
(sllgen:make-define-datatypes let-scanner-spec let-parser-spec)


;init-env: () -> Env
;usage: (init-env) = [i=1, v=5, x=10]
(define init-env
  (lambda ()
    (extend-env
     'i (num-val 1)
     (extend-env
      'v (num-val 5)
      (extend-env
       'x (num-val 10))))))


(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?)))

;expval->num : ExpVal -> Int
(define expval->num
  (lambda (val)
    (cases expval val
      (num-val (num) num)
      (else (report-expval-extractor-error 'num val)))))

;expval->bool : ExpVal -> Bool
(define expval->bool
  (lambda (val)
    (cases expval val
      (bool-val (bool) bool)
      (else (report-expval-extractor-error 'bool val)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Interpreter
;run : String -> ExpVal
(define run
  (lambda (pgm-text)
    (value-of-program (let-scan-parse pgm-text))))

;value-of-program : Program -> ExpVal
(define value-of-program
  (lambda (pgm-ast)
    (cases program pgm-ast
      (a-program (exp1) (value-of exp1 (init-env))))))

;value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp
      (const-exp (num) (num-val num))
      (var-exp (var) (apply-env env var))
      (diff-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val
                     (- num1 num2)))))
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))
      (if-exp (exp1 exp2 exp3)
              (let ((val1 (value-of exp1 env)))
                (if (expval->bool val1)
                    (value-of exp2 env)
                    (value-of exp3 env))))
      (let-exp (var exp1 body)
               (let ((var1 (value-of exp1 env)))
                 (value-of body (extend-env var var1 env)))))))
              
                
     