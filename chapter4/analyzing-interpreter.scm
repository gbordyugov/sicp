
(define (eval. exp env)
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
        ((cond?  exp)          (eval. (cond->if exp) env))
        ((or?    exp)          (eval-or  exp env))
        ((and?   exp)          (eval-and exp env))
        ((let?   exp)          (eval. (let->application  exp) env))
        ((let*?  exp)          (eval. (let*->nested-lets exp) env))
        ((while? exp)          (eval. (transform-while   exp) env))
        ((application? exp)    (apply. (eval. (operator exp) env)
                                      (list-of-values (operands exp) env)))
        (else                  (error "Unknown expression-type: EVAL" exp))))

(define apply-in-underlying-scheme apply) ;; we'll need it later

(define (apply. procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                          (procedure-parameters procedure)
                          arguments
                          (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))


(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval. (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  ;; the if-predicate is a value in the language we're implementing
  ;; and is not necessarily a Lisp boolean, that's why we're using
  ;; the `true?` predicate
  (if (true? (eval. (if-predicate exp) env))
    (eval. (if-consequent  exp) env)
    (eval. (if-alternative exp) env)))


(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval. (first-exp exps) env))
        (else             (eval. (first-exp exps) env)
                          (eval-sequence (rest-exps exps) env))))

;;
;; Assignments and definitions
;;

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval. (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (eval. (definition-value exp) env)
                    env)
  'ok)


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
(define (definition? exp)
  (tagged-list? exp 'define))

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
(define (lambda? exp)           (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)       (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; conditionals
;;
(define (if? exp)           (tagged-list? exp 'if))
(define (if-predicate exp)  (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

;;
;; this constructs a (begin ...) expression out of a list of
;; expressions, useful for cond
;;
;; omits (begin ...) if there is just one expression in the sequence
;;
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))

(define (make-begin seq) (cons 'begin seq))

;;
;; procedure application
;;
(define (application? exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;;
;; Derived expressions
;;

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause) (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))
(define (cond->if exp) (expand-clauses (cond-clauses exp)))

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
;; exercise 4.6
;;

(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (cadr exp))

(define (let-vars exp)
  (map let-binding-var (let-bindings exp)))

(define (let-body exp)
  (cddr exp))

(define (let-binding-var binding)
  (car binding))

(define (let-binding-exp binding)
  (cadr binding))

(define (let->application exp)
  (cons (make-lambda (let-vars exp)
                     (let-body exp))
        (map let-binding-exp (let-bindings exp))))

;;
;; Testing of predicates
;;

(define (true?  x) (not (eq? x false)))
(define (false? x)      (eq? x false))


;;
;; Representing procedures
;;

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;;
;; environments are lists of frames
;;

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

;;
;; frames
;;

(define (make-frame variables values) (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame))) ;; not really a data abstraction
  (set-cdr! frame (cons val (cdr frame))))

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
      (error "Unbound variable: LOOKUP-VARIABLE-VALUE:" var)
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
      (error "Unbound variable: SET!:" var)
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


(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (primitive-implementation proc)
  (cadr proc))

(define primitive-procedures
  (list (list 'car             car)
        (list 'cdr             cdr)
        (list 'cons            cons)
        (list 'null?           null?)
        (list '+               +)
        (list '-               -)
        (list '*               *)
        (list '/               /)
        (list '=               =)
        (list 'eq?             eq?)
        (list 'real-time-clock real-time-clock)
        (list 'newline         newline)
        (list 'display         display)
        ;; ... more primitives))
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))

(define  input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define the-global-environment (setup-environment))

(define tge (setup-environment))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval. input the-global-environment)))
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
               (if (eq? val '*unassigned*)
                 (error "an unassigned variable: LOOKUP-VARIABLE-VALUE:"
                        var)
                 val)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
      (error "Unbound variable: LOOKUP-VARIABLE-VALUE:" var)
      (let ((frame (first-frame env)))
        (scan (frame-variables frame)
              (frame-values    frame)))))
    (env-loop env))


;;
;; b.
;;

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
            (list nbody (cons      var  nvars) (cons val nvals)))
          (list (cons head nbody) nvars nvals)))))
  (go body))



(define (transform-body body)
  (define (make-set var val)
    (if (not (pair? var))          ;; variable or proc definition?
      (list 'set! var val)
      (let* ((proc-name (car var))
             (proc-args (cdr var)))
        (list 'set! proc-name (list 'lambda proc-args val)))))
  (define (make-let var)
    (if (not (pair? var))          ;; variable or proc definition?
      (list      var  ''*unassigned*)
      (list (car var) ''*unassigned*)))
  (let* ((new-body-vars-vals (collect-defines body))
         (new-body           (car             new-body-vars-vals))
         (vars               (cadr            new-body-vars-vals))
         (vals               (caddr           new-body-vars-vals)))
    (if (null? vars)
      body
      (let ((sets (map make-set vars vals))
            (lets (map make-let vars)))
        (list (cons 'let (cons lets (append sets new-body))))))))

;;
;; c.
;;

(define (make-procedure parameters body env)
  ;; this is incompatible with the analyzing version
  ;; (list 'procedure parameters (transform-body body) env))
  (list 'procedure parameters                 body  env))


;;
;; the analyzing part
;;
(define (eval. exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp) (analyze-self-evaluating   exp))
        ((quoted?          exp) (analyze-quoted            exp))
        ((variable?        exp) (analyze-variable          exp))
        ((assignment?      exp) (analyze-assignment        exp))
        ((definition?      exp) (analyze-definition        exp))
        ((let?             exp) (analyze
                                  (let->application        exp)))
        ((if?              exp) (analyze-if                exp))
        ((lambda?          exp) (analyze-lambda            exp))
        ((begin?           exp) (analyze-sequence 
                                  (begin-actions           exp)))
        ((cond?            exp) (analyze
                                  (cond->if                exp)))
        ((application?     exp) (analyze-application       exp))
        (else (error "Unknown expression type: ANALYZE" exp))))


(define (analyze-self-evaluating exp)
  (lambda (env) exp))


(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))


(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))


(define (analyze-assignment exp)
  (let ((var   (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))


(define (analyze-definition exp)
  (let ((var   (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))


(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate   exp)))
        (cproc (analyze (if-consequent  exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
        (cproc env)
        (aproc env)))))


(define (analyze-lambda exp)
  (let ((vars  (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env)
      (make-procedure vars bproc env))))


(define (analyze-sequence exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE"))
    (loop (car procs) (cdr procs))))


(define (analyze-application exp)
  (let ((fproc  (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env)
      (execute-application
        (fproc env)
        (map (lambda (aproc) (aproc env)) aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
          (error "Unknown procedure type: EXECUTE-APPLICATION" proc))))


;;
;; exercise 4.24
;;

;;
;; in order to run the non-analyzing version, comment out the
;; definition of analyzing eval above
;;

(define program
  '(begin
    (define (fac n)
      (if (= n 1)
        1
        (* n (fac (- n 1)))))
    (define start (real-time-clock))
    (fac 1000)
    (define end (- (real-time-clock) start))
    (newline)
    (display end)
    (newline)))

(eval. program the-global-environment)

;;
;; the difference as measured by real-time-clock is 200 msec vs 105
;; msec
;;
