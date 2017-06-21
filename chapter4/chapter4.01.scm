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

(define (let-body exp)
  (caddr exp))

(define (let-binding-var binding)
  (car binding))

(define (let-binding-exp binding)
  (cadr binding))

(define (let->application exp)
  (cons (make-lambda (map let-binding-var (let-bindings exp))
                     (let-body exp))
        (map let-binding-exp (let-bindings exp))))

(define let-test-exp '(let ((a b) (c d)) ((bla bli) (tri la))))

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
