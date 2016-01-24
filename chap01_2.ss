#lang eopl

(define list-length
  (lambda (lst)
    (if (null? lst)
        0
        (+ 1 (list-length (cdr lst))))))

(define nth-element
  (lambda (lst idx)
    (if (null? lst)
        (eopl:error 'nth-element "List too short by ~s elements.~%" (+ idx 1))
        (if (zero? idx)
            (car lst)
            (nth-element (cdr lst) (- idx 1))))))

(define nth-element-2
  (lambda (lst idx)
    (letrec ((nth-element-iter (lambda (lst1 idx1)
                              (if (null? lst1)
                                  (eopl:error 'nth-element-2 "~s does not have ~s elements.~%" lst (+ idx 1))
                                  (if (zero? idx1)
                                      (car lst1)
                                      (nth-element-iter (cdr lst1) (- idx 1)))))))
      (nth-element-iter lst idx))))
