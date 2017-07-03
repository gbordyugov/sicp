;;
;; Chapter 4 Metalinguistic Abstraction
;;

;;
;; evaluator: a procedure that, when applied to an expression,
;; performs the actions required to evaluate that expression
;;

;;
;; the most fundamental idea in programming:
;;
;; The evaluator, which determines the meaning of expressions in a
;; programming language, is just another program
;;

;;
;; (the real ;) programmers are designers of languages, rather than
;; only users of languages designed by others
;;

;;
;; 4.1 The Metacircular Evaluator
;;


;;
;; 4.1.1 The core of the Evaluator
;;


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)       (lookup-variable-value exp env))
        ((quoted? exp)         (text-of-quotation exp))
        ((assignment? exp)     (eval-assignment exp env))
        ((definition? exp)     (eval-definition exp env))
        ((if? exp)             (eval-if exp env))
        ((lambda? exp)         (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env))
        ((begin? exp)          (eval-sequence (begin-actions exp) env))
        ((cond? exp)           (eval (cond->if exp) env))
        ((application? exp)    (apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else                  (error "Unknown expression-type: EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-environment procedure))))
        (else
          (error "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ;; the if-predicate is a value in the language we're implementing
  ;; and is not necessarily a Lisp boolean, that's why we're using
  ;; the `true?` predicate
  (if (true? (eval (if-predicate exp) env))
    (eval (if-consequent  exp) env)
    (eval (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps)
         (eval (first-exp exps env)))
        (else
          (eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value env))
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval (definition-value exp) env)
                    env)
  'ok)


;;
;; exercise 4.1
;;

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((first (eval (first-operand exps) env)))
      (cons first
            (list-of-values (rest-operands exps) env)))))

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (let ((rest (list-of-values (rest-operands exps) env)))
      (cons (eval (first-operand exps) env)
            rest))))

;;
;; 4.1.2 Representing Expressions
;;

;;
;; the only self-evaluating expressions are numbers and strings
;;
(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

;;
;; variables are symbols
;;
(define (variable? exp)
  (symbol? exp))

;;
;; quotations in the form of (quote a)
;;
(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

;;
;; a little helper function
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;;
;; assignments in the form of (set! <var> <value>)
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

;;
;; definitions have the form
;; (define <var> <value>)
;; or the form
;; (define (<var> <p1> <p2> ... <pn>) <body>)
;; which is syntactic sugar for
;; (define <var> (lambda (<p1> <p2> ... <pn>) <body>))
;;
(define (definition? exp) (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
    (cadr exp)     ;; example: (define twice (lambda (x) (+ x x)))
    (caadr exp)))  ;; example: (define (twice x) (+ x x))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)  ;; example (define twice (lambda (x) (+ x x)))
    (make-lambda ;; example (define (twice x) (+ x x))
      (cdadr exp)       ;; formal parameters
      (cddr exp))))     ;; body

;;
;; lambda expressions are lists that begin with the symbol `lambda`
;;
(define (lambda? exp)
  (tagged-list? exp 'lambda))
(define (lambda-parameters exp)
  (cadr exp))
(define (lambda-body exp)
  (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; conditionals
;;
(define (if? exp)
  (tagged-list? exp 'if))
(define (if-predicate exp)
  (cadr exp))
(define (if-consequent exp)
  (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp)
  (tagged-list? exp 'begin))
(define (begin-actions exp)
  (cdr exp))
(define (last-exp? seq)
  (null? (cdr seq)))
(define (first-exp seq)
  (car seq))
(define (rest-exps seq)
  (cdr seq))

;;
;; this constructs a (begin ...) expression out of a list of
;; expressions, useful for cond
;;
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

;;
;; procedure application
;;
(define (application? exp)
  (pair? exp))
(define (operator exp)
  (car exp))
(define (operands exp)
  (cdr exp))
(define (no-operands? ops)
  (null? ops))
(define (first-operand ops)
  (car ops))
(define (rest-operands ops)
  (cdr ops))


;;
;; Derived expressions
;;

(define (cond? exp)
  (tagged-list? exp 'cond))
(define (cond-clauses exp)
  (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause)
  (car clause))
(define (cond-actions clause)
  (cdr clause))
(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest  (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
                 clauses))
        (make-if (cond-predicate first)
                 (sequence->exp (cond-actions first))
                 (expand-clauses rest))))))

;;
;; Exercise 4.2
;;

;;
;; (a) it will be evaluated as a procedure application of procedure
;; `define`
;;

;;
;; (b)
;;
(define (louis-application? exp)
  (tagged-list? exp 'call))

(define (louis-operator exp)
  (cadr exp))
(define (louis-operands exp)
  (cddr exp))
;;
;; the rest stays the same
;;
(define (louis-no-operands? ops)
  (null? ops))
(define (louis-first-operand ops)
  (car ops))
(define (louis-rest-operands ops)
  (cdr ops))


;;
;; exercise 4.3
;;

;;
;; the old version
;;
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)       (lookup-variable-value exp env))
        ((quoted? exp)         (text-of-quotation exp))
        ((assignment? exp)     (eval-assignment exp env))
        ((definition? exp)     (eval-definition exp env))
        ((if? exp)             (eval-if exp env))
        ((lambda? exp)         (make-procedure (lambda-parameters exp)
                                               (lambda-body exp)
                                               env))
        ((begin? exp)          (eval-sequence (begin-actions exp) env))
        ((cond? exp)           (eval (cond->if exp) env))
        ((application? exp)    (apply (eval (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else                  (error "Unknown expression-type: EVAL" exp))))

;;
;; the data-directed dispatch style version
;;
(define (evald exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp)       (lookup-variable-value exp env))
        ;;
        ;; so instead of
        ;;
        ;; ((quoted? exp)         (text-of-quotation exp))
        ;; ((assignment? exp)     (eval-assignment exp env))
        ;; ((definition? exp)     (eval-definition exp env))
        ;; ((if? exp)             (eval-if exp env))
        ;; ((lambda? exp)         (make-procedure (lambda-parameters exp)
        ;;                                        (lambda-body exp)
        ;;                                        env))
        ;; ((begin? exp)          (eval-sequence (begin-actions exp) env))
        ;; ((cond? exp)           (eval (cond->if exp) env))
        ;;
        ;; we just have
        ;;
        ((get-op (car exp))    ((get-op (car exp)) (cdr exp) env))
        ((application? exp)    (apply (evald (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else                  (error "Unknown expression-type: EVAL" exp))))

(define (put-op sym call)
  #t)

(define (get-op sym call)
  #f)

(define (eval-quoted exp env)
  (text-of-quotation exp))
(put-op 'quote eval-quoted)

(put-op 'set!   eval-assignment)
(put-op 'define eval-definition)
(put-op 'if     eval-definition)

(define (eval-lambda exp env)
  (make-procedure (lambda-parameters exp)
                  (lambda-body exp)
                  env))
(put-op 'lambda     eval-lambda)

(define (eval-begin exp env)
  (eval-sequence (begin-actions exp) env))
(put-op 'begin     eval-begin)

(define (eval-cond exp env)
  (evald (cond-if exp) env))
(put-op 'cond     eval-cond)


;;
;; exercise 4.4
;;

;;
;; the `and` part
;;
(define (and? exp)
  (tagged-list? exp 'and))

(define (and-clauses and-exp)
  (cdr and-exp))

(define (eval-and-clauses clauses env)
  (if (null? clauses)
    #t
    (and (eval (car clauses) exp)
         (eval-and-clauses (cdr clauses) env))))

(define (eval-and exp env)
  (eval-and-clauses (and-clauses) exp env))

;;
;; the `or` part
;;
(define (or? exp)
  (tagged-list? exp 'or))

(define (or-clauses or-exp)
  (cdr or-exp))

(define (eval-or-clauses clauses env)
  (if (null? clauses)
    #f
    (and (eval (car clauses) exp)
         (eval-or-clauses (cdr clauses) env))))

(define (eval-or exp env)
  (eval-or-clauses (or-clauses) exp env))

;;
;; to expand the body of `eval` is trivial
;;


;;
;; exercise 4.5
;;
(define (extended-cond-clause? clause)
  (eq? (cadr clause) '=>))

(define (extended-cond-clause-test clause)
  (car clause))

(define (extended-cond-clause-recepient clause)
  (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
    'false
    (let ((first (car clauses))
          (rest  (cdr clauses)))
      (if (cond-else-clause? first)
        (if (null? rest)
          (sequence->exp (cond-actions first))
          (error "ELSE clause isn't last: COND->IF"
                 clauses))
        (if (extended-cond-clause? first)
          (make-if (extended-cond-clause-test first)
                   (list (extended-cond-clause-recepient first)
                         (extended-cond-clause-test first))
                   (expand-clauses rest))
          (make-if (cond-predicate first)
                   (sequence->exp (cond-actions first))
                   (expand-clauses rest)))))))

(expand-clauses (list '(x y) '(a => b) '(else 1)))

;;
;; exercise 4.6
;;

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-vars exp)
  (map let-binding-var (let-bindings exp)))

(define (let-body exp)
  (caddr exp))

(define (let-binding-var binding)
  (car binding))

(define (let-binding-exp binding)
  (cadr binding))

(define (let->application exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (map let-binding-exp (let-bindings exp))))

(define let-test-exp '(let ((a b) (c d)) ((bla bli) (tri la))))

(let->application let-test-exp)

;;
;; addition to eval
;;
;; 
;; ((let? exp) (eval (let->application exp) env))


;;
;; exercise 4.7
;;

(define (let*? exp)
  (tagged-list? exp 'let*))

(define (let*-bindings exp)
  (cadr exp))

(define (let*-body exp)
  (cddr exp))

(define (let*-binding-var binding)
  (car binding))

(define (let*-binding-exp binding)
  (cadr binding))

(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (make-let* bindings body)
  (cons 'let* (cons bindings body)))

(define (let*->nested-lets exp)
  (if (null? (let*-bindings exp))
    (let*-body exp)
    (let* ((bindings (let*-bindings exp))
           (head (car bindings))
           (tail (cdr bindings)))
      (make-let (list head)
                (list (let*->nested-lets (make-let* tail
                                                    (let*-body exp))))))))

(let*->nested-lets '(let* ()
                     (bla bli) (tu du)))

(let*->nested-lets '(let* ((a b))
                     true (bla bli)))

(let*->nested-lets '(let* ((a b)
                           (c d))
                      true false))

(let*->nested-lets '(let* ((a b)
                           (c d)
                           (e f))
                      true false true))

;;
;; exercise 4.8
;;

(define (named-let? exp)
  (and (tagged-list? exp 'let)
       (symbol? (cadr exp))))

(define (named-let-var exp)
  (cadr exp))

(define (named-let-bindings exp)
  (caddr exp))

(define (named-let-body exp)
  (cadddr exp))

(define (named-let-binding-var binding)
  (car binding))

(define (named-let-binding-exp binding)
  (cadr binding))

(define (named-let-vars exp)
  (map named-let-binding-var (named-let-bindings exp)))

(define (named-let-exps exp)
  (map named-let-binding-exp (named-let-bindings exp)))

(define (let->application exp)
  (if (named-let? exp)
    (list
      (cons 'define
            (cons (cons (named-let-var  exp)
                        (named-let-vars exp))
                  (named-let-body exp)))
      (cons (named-let-var exp) (named-let-exps exp)))
    (cons (make-lambda (let-vars exp)
                       (let-body exp))
          (map let-binding-exp (let-bindings exp)))))

(define       let-test-exp '(let      ((a b) (c d)) ((bla bli) (tri la))))
(define named-let-test-exp '(let func ((a b) (c d)) ((bla bli) (tri la))))

(let->application let-test-exp)

(let->application named-let-test-exp)

;;
;; exercise 4.9
;;
;; I will go for a while loop with the syntax
;;
;; (while <expr> <body>)
;;
;; which is transformed to
;;
;; (begin
;;   (define (while-iter)
;;     <body>
;;     (if expr
;;       (while-iter)
;;       #t))
;;   (while-iter))
 


(define (while? exp)
  (tagged-list? exp 'while))

(define (while-clause exp)
  (cadr exp))

(define (while-body exp)
  (caddr exp))

(define (transform-while exp)
  (sequence->exp
    (list
      (list 'define '(go)
            (sequence->exp
              (list
                (while-body exp)
                (make-if (while-clause exp)
                         '(go)
                         'true))))
      (list 'go)))); call

(define while-test '(while (null? a) (repeat a)))

(transform-while while-test)

;;
;; output:
;;
;; (begin
;;   (define (go)
;;     (begin
;;       (repeat a)
;;       (if (null? a)
;;         (go)
;;         true)))
;;   (go))


;;
;; exercise 4.10 is a larger project, skipped
;;


;;
;; 4.1.3 Evaluator Data Structures
;;


;;
;; Testing of predicates
;;

(define (true?  x) (not (eq? x false)))
(define (false? x)      (eq? x false))


;;
;; Representing procedures
;;
;
;;
;; Assuming we have
;;
;; (apply-primitive-procedure <proc> <args>)
;;
;; which applies the given primitive procedure to the argument values
;; in the list <args> and returns the result of the application, and
;;
;; (primitive-procedure? <proc>)
;;
;; which tests whether <proc> is a primitive procedure.
;;


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p)
  (tagged-list? p 'procedure))

(define (procedure-parameters p)
  (cadr p))

(define (procedure-body p)
  (caddr p))

(define (procedure-environment p)
  (cadddr p))


;;
;; Operations on Environments
;;
;;
;; (lookup-variable-value <var> <env>)
;;
;; self-documented
;;
;; (extend-environment <variables> <values> <base-env>)
;;
;; self-documented
;;
;; (define-variable! <var> <value> <base-env>)
;;
;; self-documented
;;
;; (set-variable! <var> <value> <base-env>)
;;
;; self-documented
;;

;;
;; environments are lists of frames
;;

(define (enclosing-environment env)
  (cdr env))

(define (first-frame env)
  (car env))

(define the-empty-environment '())

;;
;; frames
;;

(define (make-frame variables values)
  (cons variables values))

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame))) ;; not really a data abstraction
  (set-cdr! frame (cons val (cdr frame))))

(define test-frame (make-frame '(a b c) '(1 2 3)))

(add-binding-to-frame! 'd 4 test-frame)


(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
    (cons (make-frame vars vals) base-env)
    (if (< (length vars) (length vals))
      (error "Too many arguments supplied" vars vals)
      (error "Too  few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
    (env-loop env))


(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: SET!" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
  (env-loop env))


(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars) (add-binding-to-frame! var val frame))
            ((eq? var (car vars)) (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values    frame))))


;;
;; exercise 4.11
;;

(define (frame? f)
  (tagged-list? f 'frame))

(define (make-frame variables values)
  (cons 'frame (map cons variables values)))

(define test-frame (make-frame '(a b c) '(1 2 3)))

(define (frame-variables f) (map car (cdr f)))
(define (frame-values    f) (map cdr (cdr f)))

(frame-variables test-frame)

(frame-values test-frame)

(define (add-binding-to-frame! var val frame)
  (set-cdr! frame (cons (cons var val) (cdr frame))))

(add-binding-to-frame! 'e 4 test-frame)


;;
;; exercise 4.12
;;

(define (traverse-environment var env
                              found-func
                              not-found-in-frame-func
                              not-found-in-env-func)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)          (not-found-in-frame-func frame))
            ((eq? (car vars) var)  (found-func vars vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (not-found-in-env-func env)
      (scan (frame-variables (first-frame env))
            (frame-values    (first-frame env))))))

(define (g-lookup-variable-value var env)
  (traverse-environment
    var env
    (lambda (vars vals) ;; found-func
      (car vals))
    (lambda (frame)     ;; not-found-in-frame-func
      (g-lookup-variable-value var (enclosing-environment env)))
    (lambda (env)       ;; not-found-in-env-func
      (error "Binding not found" var env))))

(define (g-set-variable-value! var val env)
  (traverse-environment
    var env
    (lambda (vars vals) ;; found-func
      (set-car! vals val))
    (lambda (frame)     ;; not-found-in-frame-func
      (g-set-variable-value! var val (enclosing-environment env)))
    (lambda (env)       ;; not-found-in-env-func
      (error "Binding not found" var env))))

(define (g-define-variable! var val env)
  (traverse-environment
    var env
    (lambda (vars vals) ;; found-func
      (set-car! vals val))
    (lambda (frame)     ;; not-found-in-frame-func
      (add-binding-to-frame! var val frame))
    'all-your-base))    ;; not-found-in-env-func, never called

;;
;; exercise 4.13
;;

;;
;; unbind just in the current frame
;;

(define make-frame cons)

(define (frame-variables frame)
  (car frame))

(define (frame-values frame)
  (cdr frame))

(define (drop-variable frame var)
  (let ((vars (frame-variables frame))
        (vals (frame-values    frame)))
    (cond ((null? vars) (error "Unbound variable" var))
          ((eq? (car vars) var) (make-frame (cdr vars) (cdr vals)))
          (else
            (let* ((res (drop-variable (make-frame (cdr vars) (cdr vals)) var))
                   (nvars (car res))
                   (nvals (cdr res)))
              (make-frame (cons (car vars) nvars)
                          (cons (car vals) nvals)))))))

(drop-variable (make-frame '(a b c) '(1 2 3)) 'b)

(drop-variable (make-frame '(a b c) '(1 2 3)) 'd)

(define (unbind-variable! var env)
  (set-car! env (drop-variable (first-frame env) var)))


;;
;; 4.1.4 Running the Evaluator as a Program
;;

(define (setup-environment)
  (let ((initial-env
          (extend-environment (primitive-procedure-names)
                              (primitive-procedure-objects)
                              the-empty-environment)))
    (define-variable! 'true  true  initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

;; (define the-global-environment (setup-environment))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car   car)
        (list 'cdr   cdr)
        (list 'cons  cons)
        (list 'null? null?)
        ;; ... more primitives))
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc)
         (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlyng-scheme
    (primitive-implementation proc) args))

(define  input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
    (display (list 'compound-procedure
                   (procedure-parameters object)
                   (procedure-body       object)
                   '<procedure-env>))
    (display object)))

(define the-global-environment (setup-environment))

;; (driver-loop)


;;
;; exercise 4.14
;;

;;
;; using native `map` assumes using native functions, but this
;; assumption doesn't hold if one uses native `map` within the
;; interpreted language
;;


;;
;; exercise 4.15
;;
;;
;; this has been discussed in `The Little Schemer`
;;

;;
;; 4.1.6 Internal Definitions
;;


;;
;; exercise 4.16
;;

;;
;; a.
;;

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars) (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (let ((val (car vals)))
               (if (eq? val *unassigned*)
                 (error "an unassigned variable" var)
                 val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
    (env-loop env))


;;
;; b.
;;

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))

;;
;; tail-recursive version, not really what we want
;;
(define (collect-defines body)
  (define (go body new-body vars vals)
    (if (null? body)
      (list new-body vars vals)
      (let* ((head (car body))
             (tail (cdr body)))
        (if (tagged-list? head 'define)
          (let ((var (cadr  head))
                (val (caddr head)))
            (go tail new-body (cons var vars) (cons val vals)))
          (go tail (cons head new-body) vars vals)))))
  (go body '() '() '()))

(define (collect-defines body)
  """ returns a list of three:
      - body stripped of defines
      - list of variables from defines
      - list of values    from defines """
  (define (go body)
    (if (null? body)
      (list '() '() '())
      (let* ((head  (car    body))
             (tail  (cdr    body))
             (nres  (go     tail))
             (nbody (car    nres))
             (nvars (cadr   nres))
             (nvals (caddr  nres)))
        (if (tagged-list? head 'define)
          (let ((var (cadr  head))
                (val (caddr  head)))
            (list nbody (cons var nvars) (cons val nvals)))
          (list (cons head nbody) nvars nvals)))))
  (go body))

(define body '((define a (lambda (x) (+ x x)))
               (define c 3) 5 (newline) 7))

(collect-defines body)


(define (transform-body body)
  (define (make-set var val)
    (list 'set! var val))
  (define (make-let var)
    (list var '*unassigned*))
  (let* ((new-body-vars-vals (collect-defines body))
         (new-body           (car             new-body-vars-vals))
         (vars               (cadr            new-body-vars-vals))
         (vals               (caddr           new-body-vars-vals))
         (sets               (map make-set vars vals))
         (lets               (map make-let vars)))
    (cons 'let (cons lets (append sets new-body)))))

(transform-body body)


;;
;; c.
;;

(define (make-procedure parameters body env)
  (list 'procedure parameters (transform-body body) env))


;;
;; exercise 4.17
;;

;;
;; let (which is a lambda application) introduces a new frame
;;

;;
;; another method would be to move all internal definitions to the
;; beginning of the body thus making sure that none of them will be
;; actually called before being defined
;;

;;
;; exercise 4.18
;;

;;
;; that wouldn't work - would break at the assignment of dy to b,
;; since y hasn't been assigned to a at this point
;;

;;
;; exercise 4.19
;;

;;
;; lots of bla-bla ;-) I'm in favour of Eva's opinion
;;
