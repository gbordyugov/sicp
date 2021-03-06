(define (eval. exp env) ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating?      exp) (analyze-self-evaluating             exp))
        ((quoted?               exp) (analyze-quoted                      exp))
        ((variable?             exp) (analyze-variable                    exp))
        ((assignment?           exp) (analyze-assignment                  exp))
        ((permanent-assignment? exp) (analyze-permanent-assignment        exp))
        ((definition?           exp) (analyze-definition                  exp))
        ((let?                  exp) (analyze
                                       (let->application                  exp)))
        ((if?                   exp) (analyze-if                          exp))
        ((if-fail?              exp) (analyze-if-fail                     exp))
        ((lambda?               exp) (analyze-lambda                      exp))
        ((begin?                exp) (analyze-sequence
                                       (begin-actions                     exp)))
        ((cond?                 exp) (analyze
                                       (cond->if                          exp)))
        ((amb?                  exp) (analyze-amb                         exp))
        ((application?          exp) (analyze-application                 exp))
        (else (error "Unknown expression type: ANALYZE"    exp))))


;;
;; Simple expressions
;;

(define (analyze-self-evaluating exp)
  (lambda (env succeed fail)
    (succeed exp fail)))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env succeed fail)
      (succeed qval fail))))

(define (analyze-variable exp)
  (lambda (env succeed fail)
    (succeed (lookup-variable-value exp env) fail)))

(define (analyze-lambda exp)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (lambda (env succeed fail)
      (succeed (make-procedure vars bproc env) fail))))

;;
;; Conditionals and sequences
;;

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate   exp)))
        (cproc (analyze (if-consequent  exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             ;; success continuation for evaluating the predicate
             ;; to obtain pred-value
             (lambda (pred-value fail2)
               (if (true? pred-value)
                 (cproc env succeed fail2)
                 (aproc env succeed fail2)))
             ;; failure continuation for evaluating the predicate
             fail))))

(define (analyze-sequence exps)
  (define (sequentially a b)
    (lambda (env succeed fail)
      (a env
         ;; success continuation for calling a
         (lambda (a-value fail2)
           (b env succeed fail2))
         ;; failure continuation for calling a
         fail)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
      first-proc
      (loop (sequentially first-proc (car rest-procs))
            (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
      (error "Empty sequence: ANALYZE-SEQUENCE"))
    (loop (car procs) (cdr procs))))


;;
;; Definitions and assignments
;;

(define (analyze-definition exp)
  (let ((var   (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (define-variable! var val env)
               (succeed 'ok fail2))
             fail))))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok
                          (lambda ()
                            (set-variable-value! var old-value env)
                            (fail2)))))
             fail))))

;;
;; Procedure application
;;

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (lambda (env succeed fail)
      (fproc env
             (lambda (proc fail2)
               (get-args aprocs env
                         (lambda (args fail3)
                           (execute-application proc args succeed fail3))
                         fail2))
             fail))))

(define (get-args aprocs env succeed fail)
  (if (null? aprocs)
    (succeed '() fail)
    ((car aprocs)
     env
     ;; success continuation
     (lambda (arg fail2)
       (get-args
         (cdr aprocs)
         env
         (lambda (args fail3)
           (succeed (cons arg args) fail3))
         fail2))
     fail)))

(define (execute-application proc args succeed fail)
  (cond ((primitive-procedure? proc)
         (succeed (apply-primitive-procedure proc args) fail))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))
          succeed
          fail))
        (else
          (error "Unknown procedure type: EXECUTE-APPLICATION"
                 proc))))

;;
;; Evaluating amb expressions
;;

(define (analyze-amb exp)
  (let ((cprocs (map analyze (amb-choices exp))))
    (lambda (env succeed fail)
      (define (try-next choices)
        (if (null? choices)
          (fail)
          ((car choices) env succeed (lambda ()
                                       (try-next (cdr choices))))))
      (try-next cprocs))))


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
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))

;;
;; definitions
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
(define (lambda? exp)           (tagged-list? exp 'lambda))
(define (lambda-parameters exp) (cadr exp))
(define (lambda-body exp)       (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;;
;; conditionals
;;
(define (if?            exp) (tagged-list? exp 'if))
(define (if-predicate   exp) (cadr exp))
(define (if-consequent  exp) (caddr exp))
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
;; ambivalent expressions
;;
(define (amb? exp) (tagged-list? exp 'amb))
(define (amb-choices exp) (cdr exp))

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

(define (let? exp) (tagged-list? exp 'let))
(define (let-bindings exp) (cadr exp))
(define (let-vars exp) (map let-binding-var (let-bindings exp)))
(define (let-body exp) (cddr exp))
(define (let-binding-var binding) (car binding))
(define (let-binding-exp binding) (cadr binding))

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

(define (make-procedure parameters body env)
  (list 'procedure parameters                 body  env))


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

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((memq (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define primitive-procedures
  (list (list 'car             car)
        (list 'cdr             cdr)
        (list 'cadr            cadr)
        (list 'cons            cons)
        (list 'null?           null?)
        (list 'integer?        integer?)
        (list '+               +)
        (list '-               -)
        (list '*               *)
        (list '/               /)
        (list 'sqrt            sqrt)
        (list '=               =)
        (list 'eq?             eq?)
        (list '>               >)
        (list '>=              >=)
        (list '<               <)
        (list '<=              <=)
        (list 'not             not)
        (list 'abs             abs)
        (list 'real-time-clock real-time-clock)
        (list 'newline         newline)
        (list 'display         display)
        (list 'list            list)
        (list 'member          member)
        (list 'memq            memq)
        (list 'distinct?       distinct?)
        (list 'remainder       remainder)
        ;; ... more primitives))
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define apply-in-underlying-scheme apply)

(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
    (primitive-implementation proc) args))


;;
;; driver loop
;;

(define input-prompt   ";;; Amb-Eval input:")
(define output-prompt  ";;; Amb-Eval value:")

(define (driver-loop)
  (define (internal-loop try-again)
    (prompt-for-input input-prompt)
    (let ((input (read)))
      (if (eq? input 'try-again)
        (try-again)
        (begin
          (newline) (display ";;; Starting a new problem ")
          (ambeval input the-global-environment
                   (lambda (val next-alternative)
                     (announce-output output-prompt)
                     (user-print val)
                     (internal-loop next-alternative))
                   (lambda ()
                     (announce-output ";;; There are no more values of")
                     (user-print input)
                     (driver-loop)))))))
  (internal-loop
    (lambda ()
      (newline) (display ";;; There is no current problem")
      (driver-loop))))

(define (ambeval exp env succeed fail)
  ((analyze exp) env succeed fail))

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
(define tge (setup-environment))

(define (gambeval exp)
  (let ((new-exp (list 'begin
                       '(define (require p) (if (not p) (amb)))
                       exp)))
    (ambeval new-exp
             the-global-environment
             (lambda (value fail) value)
             (lambda () 'failed))))


;;
;; from exercise 4.51
;;

(define (permanent-assignment? exp) (tagged-list? exp 'permanent-set!))
(define (permanent-assignment-variable exp) ( cadr exp))
(define (permanent-assignment-value    exp) (caddr exp))

(define (analyze-permanent-assignment exp)
  (let ((var            (permanent-assignment-variable exp))
        (vproc (analyze (permanent-assignment-value exp))))
    (lambda (env succeed fail)
      (vproc env
             (lambda (val fail2)
               (let ((old-value (lookup-variable-value var env)))
                 (set-variable-value! var val env)
                 (succeed 'ok fail2)))
             fail))))


;;
;; from exercise 4.52
;;

(define (if-fail? exp)
  (tagged-list? exp 'if-fail))

(define (if-fail-predicate   exp) ( cadr exp))
(define (if-fail-alternative exp) (caddr exp))

(define (analyze-if-fail exp)
  (let ((pproc (analyze (if-fail-predicate   exp)))
        (aproc (analyze (if-fail-alternative exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (x fail2)
               (succeed x fail2))
             (lambda ()
               (aproc env succeed fail))))))
