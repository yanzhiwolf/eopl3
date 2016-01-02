;(load "sllgen/chez-init.ss")

;;; (define-datatype name
;;;  (variant-name (field-name predicate-exp) ...)
;;;  ...)
;;; * variant-names become constructors
;;; * The field-names are used only for debugging.

(define-datatype lc-exp lc-exp?
  (var-exp
   (var symbol?))
  (lambda-exp
   (bound-var symbol?)
   (body lc-exp?))
  (app-exp
   (rator lc-exp?)
   (rand lc-exp?)))

;;;test
(define occurs-free?
  (lambda (search-var exp)
    (cases lc-exp exp
      (var-exp (var)
               (eqv? var search-var))
      (lambda-exp (bound-var body)
                  (and
                   (not (eqv? search-var bound-var))
                   (occurs-free? search-var body)))
      (app-exp (rator rand)
               (or
                (occurs-free? search-var rator)
                (occurs-free? search-var rand))))))

(occurs-free? 'a (var-exp 'a))