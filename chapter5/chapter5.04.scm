;;
;; started Chapter 5.4 The Explicit-Control Evaluator
;;

;;
;; Aim of this chapter: show how implement the abstraction of
;; procedure definition and calling using register machines
;;

;;
;; Registers and Operations

;;
;; Scheme evaluator includes a stack plus the registers: exp, env,
;; val, continue, proc, arg1, and unev.
;;
;; exp is used to hold the expression to be evaluated
;; env holds the corresponding environment
;; val contains the value obtained by evaluating the expression in the
;;     designed environment
;; continue register is used to implement recursion in the evaluator
;;     itself
;; proc, arg1, and unev are used in evaluating combinations.
;;

;;
;; 5.4.1 The core of the Explicit-Control Evaluator
;;

eval-dispatch
  (test (op self-evaluating?) (reg exp))
  (branch (label ev-self-eval))
  (test (op variable?) (reg exp))
  (branch (label ev-variable))
  (test (op quoted?) (reg exp))
  (branch (label ev-quoted))
  (test (op assignment?) (reg exp))
  (branch (label ev-assignment))
  (test (op definition?) (reg exp))
  (branch (label ev-definition))
  (test (op if?) (reg exp))
  (branch (label ev-if))
  (test (op lambda?) (reg exp))
  (branch (label eval-lambda))
  (test (op begin?) (reg exp))
  (branch (label ev-begin))
  (test (op application?) (reg exp))
  (branch (label ev-application))
  (goto (label unknown-expression-type))


;;
;; Evaluating simple expressions
;;

ev-self-eval
  (assign val (reg exp))
  (goto (reg continue))

ev-variable
  (assign val (op lookup-variable-value) (reg exp) (reg env))
  (goto (reg continue))

ev-quoted
  (assign val (op text-of-quotation) (reg exp))
  (goto (reg continue))

ev-lambda
  (assign unev (op lambda-parameters) (reg exp))
  (assign exp (op lambda-body) (reg exp))
  (assign val (op make-procedure) (reg unev) (reg exp) (reg env))
  (goto (reg continue))

;; observe that unev and exp hold the parameters and the body of the
;; lambda expression so that they can be passed to make-procedure,
;; along with the environment in env

;;
;; Evaluating procedure application
;;

;;
;; the explicit-control evaluator first evaluates the arguments of the
;; procedure by calling itself in a recursive manner.
;;
;; we begin the evaluation of an application by evaluating the
;; operator to produce a procedure, which will later be applied to the
;; evaluated operands. To evaluate the operator, we move it to the exp
;; register and go to eval-dispatch. The environment in the env
;; register is already the correct one in which to evaluate the
;; operator. We however save en because we will need it later to
;; evaluate the operands. We also extract the operands into unev and
;; save this on the stack. The register continue is set up in the way
;; that eval-dispatch will resume at ev-appl-did-operator after the
;; operator has been evaluated. First, however, we save the old value
;; of continue, which tells the controller where to go after the
;; application
;;

ev-application
  (save continue)
  (save env)
  (assign unev (op operands) (reg exp))
  (save unev)
  (assign exp (op operator) (reg exp))
  (assign continue (label ev-appl-did-operator))
  (goto (label eval-dispatch))
