;empty-env: () -> Env
(define empty-env
  (lambda () '()))


;extend-env: Var * SchemeVal * Env -> Env
(define extend-env
  (lambda (var val env)
    (cons (cons var val) env)))

;apply-env: Var * Env -> SchemeVal
(define apply-env
  (lambda (env search-var)
    (cond
      ((null? env) (report-no-binding-found search-var))
      ((eqv? (caar env) search-var) (cdar env))
      (else
       (apply-env (cdr env) search-var)))))

(define report-no-binding-found
  (lambda (search-var)
    (begin
      (display "no binding for ")
      (display search-var)
      (newline))))