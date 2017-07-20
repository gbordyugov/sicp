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
        ((application? exp)    (apply. (actual-value (operator exp) env)
                                       (operands exp)
                                       env))
        (else                  (error "Unknown expression-type: EVAL" exp))))

(define apply-in-underlying-scheme apply) ;; we'll need it later

(define (actual-value exp env)
  (force-it (eval. exp env)))

(define (apply. procedure arguments env)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
           procedure
           (list-of-arg-values arguments env))) ;; primitives are strict
        ((compound-procedure? procedure)
         (eval-sequence (procedure-body procedure)
                        (extend-environment
                          (procedure-parameters procedure)
                          (list-of-delayed-args arguments env)
                          (procedure-environment procedure))))
        (else (error "Unknown procedure type: APPLY" procedure))))

;;
;; representing thunks
;;

(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk)
  (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj) (let ((result (actual-value (thunk-exp obj)
                                                  (thunk-env obj))))
                        (set-car! obj 'evaluated-thunk)
                        (set-car! (cdr obj) result)
                        (set-cdr! (cdr obj) '())
                        result))
        ((evaluated-thunk? obj) (thunk-value obj))
        (else obj)))
           

(define (list-of-values exps env)
  (if (no-operands? exps)
    '()
    (cons (eval. (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))

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
  (if (true? (actual-value (if-predicate exp) env))
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
;;
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
  (if (not (null? (cdddr exp))) (cadddr exp) 'false))

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
;; exercise 4.4
;;

;;
;; the `and` part
;;
(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses and-exp) (cdr and-exp))

(define (eval-and-clauses clauses env)
  (if (null? clauses)
    #t
    (and (eval. (car clauses) exp)
         (eval-and-clauses (cdr clauses) env))))

(define (eval-and exp env) (eval-and-clauses (and-clauses) exp env))

;;
;; the `or` part
;;
(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses or-exp) (cdr or-exp))

(define (eval-or-clauses clauses env)
  (if (null? clauses)
    #f
    (and (eval. (car clauses) exp)
         (eval-or-clauses (cdr clauses) env))))

(define (eval-or exp env) (eval-or-clauses (or-clauses) exp env))

;;
;; exercise 4.5
;;
(define (extended-cond-clause? clause) (eq? (cadr clause) '=>))
(define (extended-cond-clause-test clause) (car clause))
(define (extended-cond-clause-recepient clause) (caddr clause))

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
;;


(define (make-procedure parameters body env)
  (list 'procedure parameters body env))

(define (compound-procedure? p) (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))


;;
;; Operations on Environments
;;

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

;; (define test-frame (make-frame '(a b c) '(1 2 3)))

;; (add-binding-to-frame! 'd 4 test-frame)


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


(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

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

(define  input-prompt ";;; L-Eval input:")
(define output-prompt ";;; L-Eval value:")

(define the-global-environment (setup-environment))

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (actual-value input the-global-environment)))
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

(define prog
  '(begin
     (define (cons x y) (lambda (m) (m x y)))
     (define (car    z) (z (lambda (p q) p)))
     (define (cdr    z) (z (lambda (p q) q)))

     (define (list-ref items n)
       (if (= n 0)
         (car items)
         (list-ref (cdr items) (- n 1))))

     (define (map proc items)
       (if (null? items)
         '()
         (cons (proc (car items))
               (map proc (cdr items)))))

     (define (scale-list items factor)
       (map (lambda (x) (* x factor)) items))

     (define (add-lists list1 list2)
       (cond ((null? list1) list2)
             ((null? list2) list1)
             (else (cons (+ (car list1) (car list2))
                         (add-lists (cdr list1) (cdr list2))))))

     (define ones (cons 1 ones))
     (define integers (cons 1 (add-lists ones integers)))
     (newline)
     (display (list-ref integers 17))
     ))

;; (eval. prog the-global-environment)



(define prog
  '(begin
     (define (cons x y) (lambda (m) (m x y)))
     (define (car    z) (z (lambda (p q) p)))
     (define (cdr    z) (z (lambda (p q) q)))

     (define (list-ref items n)
       (if (= n 0)
         (car items)
         (list-ref (cdr items) (- n 1))))

     (define (map proc items)
       (if (null? items)
         '()
         (cons (proc (car items))
               (map proc (cdr items)))))

     (define (scale-list items factor)
       (map (lambda (x) (* x factor)) items))

     (define (add-lists list1 list2)
       (cond ((null? list1) list2)
             ((null? list2) list1)
             (else (cons (+ (car list1) (car list2))
                         (add-lists (cdr list1) (cdr list2))))))

     (define (integral integrand initial-value dt)
       (define int
         (cons initial-value
               (add-lists (scale-list integrand dt) int)))
       int)

     (define (solve f y0 dt)
       (define y (integral dy y0 dt))
       (define dy (map f y))
       y)

     (newline)
     (display (list-ref (solve (lambda (x) x) 1.0 0.001) 1000))
     ))

(eval. prog the-global-environment)
