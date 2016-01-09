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
    (expression ("*" "(" expression "," expression ")") mul-exp)
    (expression ("/" "(" expression "," expression ")") div-exp)
    (expression ("+" "(" expression "," expression ")") add-exp)
    (expression ("-" "(" expression "," expression ")") sub-exp)
    (expression ("if" expression "then" expression "else" expression) if-exp)
    (expression ("cond" (arbno expression "==>" expression) "end") cond-exp)
    (expression ("let" (arbno identifier "=" expression) "in" expression) let-exp)
    (expression ("let*" (arbno identifier "=" expression) "in" expression) let*-exp)
    (expression ("minus" "(" expression ")") minus-exp)
    (expression ("zero?" "(" expression ")") zero?-exp)
    (expression ("equal?" "(" expression "," expression ")") equal?-exp)
    (expression ("greater?" "(" expression "," expression ")") greater?-exp)
    (expression ("less?" "(" expression "," expression ")") less?-exp)
    (expression ("list" "(" (separated-list expression ",") ")") list-exp)
    (expression ("cons" "(" expression "," expression ")") cons-exp)
    (expression ("car" "(" expression ")") car-exp)
    (expression ("cdr" "(" expression ")") cdr-exp)
    (expression ("null?" "(" expression ")") null?-exp)
    (expression ("emptylist") emptylist-exp)
    (expression ("print" "(" expression ")") print-exp)))

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
       'x (num-val 10)
       (empty-env))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;the expressed value for LET language
;an expressed value is either a number, a boolean or a list
(define-datatype expval expval?
  (num-val (num number?))
  (bool-val (bool boolean?))
  (list-val (lst list?)))

;;; Extractors:
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

;expval->list : ExpVal -> List
(define expval->list
  (lambda (val)
    (cases expval val
      (list-val (lst) lst)
      (else (report-expval-extractor-error 'list val)))))


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

      ;(expression (number) const-exp)
      (const-exp (num) (num-val num))

      ;(expression (identifier) var-exp)
      (var-exp (var) (apply-env env var))

      (mul-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val (* num1 num2)))))

      (div-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val (/ num1 num2)))))
      
      (add-exp (exp1 exp2)
               (let ((val1 (value-of exp1 env))
                     (val2 (value-of exp2 env)))
                 (let ((num1 (expval->num val1))
                       (num2 (expval->num val2)))
                   (num-val (+ num1 num2)))))
      
      (sub-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (let ((num1 (expval->num val1))
                        (num2 (expval->num val2)))
                    (num-val (- num1 num2)))))

      (minus-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (num-val (- num1)))))
      
      (zero?-exp (exp1)
                 (let ((val1 (value-of exp1 env)))
                   (let ((num1 (expval->num val1)))
                     (if (zero? num1)
                         (bool-val #t)
                         (bool-val #f)))))

      (equal?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (= num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))

      (greater?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (> num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))

      (less?-exp (exp1 exp2)
                  (let ((val1 (value-of exp1 env))
                        (val2 (value-of exp2 env)))
                    (let ((num1 (expval->num val1))
                          (num2 (expval->num val2)))
                      (if (< num1 num2)
                          (bool-val #t)
                          (bool-val #f)))))

      ;(expression ("list" "(" (separated-list expression ",") ")") list-exp)
      (list-exp (args)
                (list-val (map ((lambda (e)
                                 (lambda (elem)
                                   (value-of elem e))) env) args)))
      
      ;(expression ("cons" "(" expression "," expression ")") cons-exp)
      (cons-exp (exp1 exp2)
                (let ((val1 (value-of exp1 env))
                      (val2 (value-of exp2 env)))
                  (list-val (list val1 val2))))
      
      ;(expression ("car" "(" expression ")") car-exp)
      (car-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (car lst1))))
      
      ;(expression ("cdr" "(" expression ")") cdr-exp)
      (cdr-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (cdr lst1))))

      ;(expression ("null?" "(" expression ")") null?-exp)
      (null?-exp (exp1)
               (let ((val1 (value-of exp1 env)))
                 (let ((lst1 (expval->list val1)))
                   (null? lst1))))

      ;(expression ("emptylist" emptylist-exp)
      (emptylist-exp ()
                     (list-val '()))

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
                 (letrec ((bind-let
                           (lambda (varlst vallst env-let)
                             (if (null? varlst)
                                 env-let
                                 (let ((var (car varlst))
                                       (val (car vallst)))
                                   (bind-let (cdr varlst) (cdr vallst) (extend-env var val env-let)))))))
                   (value-of body (bind-let vars vals env)))))

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

      ;(expression ("print" "(" expression ")") print-exp)
      (print-exp (exp1)
                 (let ((var1 (value-of exp1 env)))
                   (begin (display var1) (num-val 1)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;test
(run "5")
(run "i")
(run "-(i, -(v, x))")
(run "if zero?(0) then 1 else 2")
(run "let y = -(i, -(v,x)) in y")
(run "equal?(2,3)")
(run "greater?(2,3)")
(run "less?(2,3)")
(run "cons (1,2)")
(run "car (cons (1, 2))")
(run "cdr (cons (1, 2))")
(run "emptylist")
(run "null? (emptylist)")
(run "let x=4 in cons(x, cons(cons(-(x,1),emptylist),emptylist))")
(run "list(1,2,3)")
(run "let x=1 in list(x, -(4,x), minus(x))")
(run "cond less?(2,3) ==> list(1,2,3) end")
(run "print(2)")
(run "print(list(1,2,3))")
(run "let x=30 in let x = -(x,1) y = -(x,2) in -(x, y)")   ;=1
(run "let x=30 in let* x = -(x,1) y = -(x,2) in -(x,y)")  ;=2