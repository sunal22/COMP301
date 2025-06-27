#lang eopl

;; interpreter for the LET language

(require "lang.rkt")
(require "data-structures.rkt")
(require "environments.rkt")

(provide value-of-program value-of)

;;;;;;;;;;;;;;;; the interpreter ;;;;;;;;;;;;;;;;

;; value-of-program : Program -> ExpVal
(define value-of-program 
  (lambda (pgm)
    (cases program pgm
      (a-program (exp1)
                 (value-of exp1 (init-env))))))

;; value-of : Exp * Env -> ExpVal
(define value-of
  (lambda (exp env)
    (cases expression exp

      (const-exp (num)
        (num-val num))

      (var-exp (var)
        (apply-env env var))

      (list-exp () (list-val '())) ; returns an empty list

      (cons-exp (exp1 exp2)
        (let ((val1 (value-of exp1 env))
              (val2 (value-of exp2 env)))
          (let ((n (expval->num val1))
                (lst (expval->list val2)))
            (list-val (cons n lst)))))

      (mul-exp (lst-exp)
        (let ((val (value-of lst-exp env)))
          (let ((lst (expval->list val)))
            (if (null? lst)
                (num-val 0)
                (num-val (apply * lst))))))

      (op-exp (exp1 exp2 op)
              (let ((val1 (value-of exp1 env))
                    (val2 (value-of exp2 env)))
                  (let ((num1 (expval->rational val1))
                        (num2 (expval->rational val2)))
                      (cond 
                        ((and (number? num1) (number? num2))
                          (num-val
                            (cond 
                              ((= op 1) (+ num1 num2))
                              ((= op 2) (* num1 num2))
                              ((= op 3) (/ num1 num2))
                              (else (- num1 num2))
                              )))
                        
                        ((and (number? num1) (not (number? num2)))
                          (rational-val
                          (let ((num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1 num2bot) num2top) num2bot))
                              ((= op 2) (cons (* num1 num2top) num2bot))
                              ((= op 3) (cons (* num1 num2bot) num2top))
                              (else (cons (- (* num1 num2bot) num2top) num2bot)))

                              
                              )))

                        ((and (number? num2) (not (number? num1)))
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1)))
                            (cond 
                              ((= op 1) (cons (+ (* num1bot num2) num1top) num1bot))
                              ((= op 2) (cons (* num1top num2) num1bot))
                              ((= op 3) (cons num1top (* num1bot num2)))
                              (else (cons (- num1top (* num1bot num2)) num1bot))))))

                        (else
                          (rational-val
                          (let ((num1top (car num1))
                                (num1bot (cdr num1))
                                (num2top (car num2))
                                (num2bot (cdr num2)))
                            (cond 
                              ((= op 1) (cons (+ (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot))) ;; add
                              ((= op 2) (cons (* num1top num2top) (* num1bot num2bot))) ;; multiply
                              ((= op 3) (cons (* num1top num2bot) (* num1bot num2top)))
                              (else (cons (- (* num1top num2bot) (* num1bot num2top)) (* num1bot num2bot)))
      
                            ))))))))

      (zero?-exp (exp1)
        (let ((val1 (value-of exp1 env)))
          (let ((num1 (expval->rational val1)))
            (if (number? num1)
                (bool-val (zero? num1))
                (bool-val (zero? (car num1)))))))

      

      (let-exp (var exp1 body)
        (let ((val1 (value-of exp1 env)))
          (value-of body (extend-env var val1 env))))


      (simpl-exp (exp1)
                 (let ((val (value-of exp1 env)))
                   (let ((rat (expval->rational val)))
                     (if (number? rat)
                         (num-val rat)
                         (let* ((num (car rat))
                                (den (cdr rat))
                                (g   (gcd num den)))
                           (rational-val (cons (/ num g) (/ den g))))))))

      (rational-exp (num1 num2)
                    (if (= num2 0)
                        (eopl:error "Division by zero in rational-exp")
                        (rational-val (cons num1 num2))))


     (min-exp (lst)
               (let ((lv (value-of lst env)))
                     (let ((nums (expval->list lv)))
                       (num-val (if (null? nums) -1 (apply min nums))))))
                     
      
      (if-elif-exp (exp1 exp2 exp3 exp4 exp5)
                   (let ((b1 (expval->bool (value-of exp1 env)))
                         (b2 (expval->bool (value-of exp3 env))))
                     (if b1
                         (value-of exp2 env)
                         (if b2
                             (value-of exp4 env)
                             (value-of exp5 env)))))
)))