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

(define  input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input-prompt (read)))
    (let ((output (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define the-global-environment (setup-environment))

;; (driver-loop)


;;
;; Representing thunks
;;

(define (force-it obj)
  (if (thunk? obj)
    (actual-vaue (thunk-exp obj) (thunk-env obj))
    obj))


(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thung))

(define (thunk-exp t) (cadr  t))
(define (thunk-env t) (caddr t))

;;
;; memoizing thunks
;;

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))

(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value (thunk-exp obj)
                                     (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr ojb) result) ;; replace with the value
           (set-car! (cdr obj) '())    ;; forget unneeded env
           result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))


;;
;; exercise 4.27
;;

;;
;; count would be incremented on forcing w
;;


;;
;; exercise 4.28
;;

;;
;; procedure values must be fully forced before applying them to their
;; arguments
;;

;;
;; exercise 4.29
;;

;;
;; factorial would be a nice example of that
;;


;;
;; exercise 4.30
;;

;;
;; (a) applications of primitive functions are always forced, see
;; the thunks rules above
;;

;;
;; (b) depending on forced or not, (p2 1) returns either 1 or (1 2)
;;


;;
;; (c) (display) is a primitive function
;;

;;
;; (d) Cy D Effect seems logical to me here
;;


;;
;; exercise 4.31 skipped
;;


;;
;; 4.2.3 Streams as Lazy Lists
;;


;;
;; Exercise 4.32
;;

;;
;; Infinite trees, i.e. in min-max algorithms
;;


;;
;; Exercise 4.33
;;

;;
;; turn list into series of applications of cons
;;
(define (make-lazy-list elems)
  (if (null? elems)
    '()
    (list 'cons (car elems) make-lazy-list (cdr elems))))

;; extracts (a b c) from (quote (a b c))
;; old version
(define (text-of-quotation exp)
  (cadr exp))

;; extracts (a b c) from (quote (a b c))
;; new version
(define (text-of-quotation exp env)
  (let ((quoted (cadr exp)))
    (if (pair? quoted)
      (eval (make-lazy-list quoted) env)
      quoted)))


;;
;; exercise 4.34
;;

;;
;; a common strategy is to try to print up to N first elements of the
;; list and then print an ellipsis ...
;;
