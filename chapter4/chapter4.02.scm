;;
;; 4.2 Variations on a Scheme - Lazy Evaluation
;;

;;
;; exercise 4.25
;;

;;
;; this would result in an infinite loop
;;


;;
;; exercise 4.26
;;

(define (unless? exp)
  (tagged-list exp 'unless))

(define (unless-condition exp)
  (cadr exp))

(define (unless-condition-usual-value exp)
  (caddr exp))

(define (unless-condition-exceptional-value exp)
  (cdddr exp))

(define (eval-unless exp env)
  (if (true? (eval (unless-condition exp) env))
    (eval (unless-condition-exceptional-value  exp) env)
    (eval (unless-condition-usual-value exp) env)))

;;
;; there are probably some exotic uses of unless as a higher-order
;; procedure, cannot think of them now
;;


;;
;; 4.2.2 An Interpreter with Lazy Evalution
;;

;;
;; the lazy arguments of a procedure is transformed into thunks upon
;; the call.
;;
;; Thunk: an expression together with the environment, in which the
;; procedure application is being created
;;
;; Thunk is forced when:
;;  - it is passed to a primitive procedure
;;  - when it is the value of a predicate in a conditional
;;  - when it is the value of an operator that is about to be applied
;;  as a procedure
;;

;;
;; modified procedure application
;;
;; ((application? exp)
;;  (apply (actual-value (operator exp) env)
;;         (operands exp)
;;         env))


(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply. procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env))) ;; changed
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             (list-of-delayed-args arguments env)   ;; changed
             (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
    '()
    (cons (actual-value (first-operand exps) env)
          (list-of-arg-values (rest-operands exps) env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
    '()
    (cons (delay-it (first-operand exps) env)
          (list-of-delayed-args (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (actual-value (if-predicateexp) env))
    (eval (if-consequent  exp) env)
    (eval (if-alternative exp) env)))
