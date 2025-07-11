#lang eopl

;; builds environment interface, using data structures defined in
;; data-structures.rkt. 

(require "data-structures.rkt")

(provide init-env empty-env extend-env apply-env)

;;;;;;;;;;;;;;;; initial environment ;;;;;;;;;;;;;;;;

;; init-env : () -> Env
;; usage: (init-env) = [i=1, v=5, x=10]
;; (init-env) builds an environment in which i is bound to the
;; expressed value 1, v is bound to the expressed value 5, and x is
;; bound to the expressed value 10.
;; Page: 69

;;; initialize environment with x, y, z
(define init-env 
  (lambda ()
     (extend-env
      'v (num-val 3)
      ;; -----------------------
      ;; INSERT YOUR CODE HERE 
      ;; -----------------------
      (extend-env
       'z (num-val 3)
       (extend-env
        'y (num-val 2)
        (extend-env
         'x (num-val 4)
         (empty-env))))
      )))
      ;; -----------------------

;;;;;;;;;;;;;;;; environment constructors and observers ;;;;;;;;;;;;;;;;

(define empty-env
  (lambda ()
    (empty-env-record)))

(define empty-env? 
  (lambda (x)
    (empty-env-record? x)))

(define extend-env
  (lambda (sym val old-env)
    (extended-env-record sym val old-env)))

(define apply-env
  (lambda (env search-sym)
    (if (empty-env? env)
        (eopl:error 'apply-env "No binding for ~s" search-sym)
        (let ((sym (extended-env-record->sym env))
              (val (extended-env-record->val env))
              (old-env (extended-env-record->old-env env)))
          (if (eqv? search-sym sym)
              val
              (apply-env old-env search-sym))))))