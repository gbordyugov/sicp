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

ev-appl-did-operator
  (restore unev)
  (restore env)
  (assign arg1 (op empty-arglist))
  (assign proc (reg val)) ;; this one was returned from eval-dispatch
  (test (op no-operands?) (reg unev))
  (branch (label apply-dispatch))
  (save proc) ;; go into the loop from here, see below

;;
;; here comes the loop: we evaluate an operand from the list in unev
;; and accumulate the result into arg1. The evaluation is done by
;; placing it in the exp register and calling eval-dispatch, after
;; setting continue so that the execution will resume with the
;; argument-accumulation phase. But first we save the arguments
;; accumulated so far (held in arg1), the environment (held in env),
;; and the remaining operand to be evaluated (held in unev). A special
;; case is made for the evaluation of the last operand, which is
;; handled at ev-appl-last-arg.
;;

ev-appl-operand-loop
  (save arg1)
  (assign exp (op first-operand) (reg unev))
  (test (op last-operand?) (reg unev))
  (branch (label ev-appl-last-arg))
  (save env)
  (save unev)
  (assign continue (label av-appl-accumulate-arg))
  (goto (label eval-dispatch))

;;
;; When an operand has been evaluated, the value is accumulated into
;; the list held in arg1. The operand is then removed from the list of
;; unevaluated operands in unev, and the argument-evaluation
;; continues.
;;

ev-appl-accumulate-arg
  (restore unev)
  (restore env)
  (restore arg1)
  (assign arg1 (op adjoin-arg) (reg val) (reg arg1))
  (assign unev (op rest-operands) (reg unev))
  (goto (label ev-appl-operand-loop))

;;
;; Evaluation of the last argument is handled differently. There is no
;; need to save the environmen t or the list of unevaluated operands
;; before going to eval-dispatch, since they will not be required
;; after the last operand is evaluated. Thus we return from the
;; evaluation to a special entry point `ev-appl-accum-last-arg`, which
;; restores the argument list, accumulates the new argument, restores
;; the saved procedure, and goes off to perform the application
;;

ev-appl-last-arg
  (assign continue (label ev-appl-accum-last-arg))
  (goto (label eval-dispatch))

ev-appl-accum-last-arg
  (restore arg1)
  (assign arg1 (op adjoin-arg) (reg val) (reg arg1))
  (restore proc)
  (goto (label apply-dispatch))

;;
;; a couple of notes on the order, in which the argument list is
;; evaluated
;;


;;
;; Procedure application
;;

;;
;; By the time we get to apply-dispatch (see below), the proc register
;; contains the procedure to apply and arg1 contains the list of
;; evaluated arguments to which it must be applied. The saved value of
;; `continue` (originally passed to eval-dispatch and saved at
;; ev-application), which tells where to return with the result of the
;; procedure application, is on the stack. When the application is
;; complete, the controller transfers to the entry point specified by
;; the saved `continue`, with the result of the application in val. As
;; with the metacircular `apply`, there are two cases to consider:
;; Either the procedure to be applied is a primitive or it is a
;; compound procedure.
;;

apply-dispatch
  (test (op primitive-procedure?) (reg proc))
  (branch (label primitive-apply))
  (test (op compound-procedure?) (reg proce))
  (branch (label compound apply))
  (goto (label unknown-procedure-type))

;;
;; we assume that both primitives `primitive-procedure?` and
;; `compound-procedure?` exist or implemented in the system
;;

primitive-apply
  (assing val (op apply-primitve-procedure)
          (reg proc)
          (reg arg1))
  (restore continue)
  (goto (reg continue))

;;
;; to apply a compound procedure, we proceed just as with the
;; metacircular evaluator. We construct a frame that binds the
;; procedure's parameters to the arguments, use this fram to extend
;; the environment carried by the procedure, and evaluate in this
;; extended environment the sequence of expressions that forms the
;; body of the procedure. `ev-sequence`, described later on, handles
;; the evaluation of the sequence.
;;

compound-apply
  (assign unev (op procedure-parameters) (reg proc))
  (assign env (op procedure-environment) (reg proc))
  (assign env (op extend-environment)
          (reg unev) (reg arg1) (reg env))
  (assign unev (op precedure-body) (reg proc))
  (goto (label ev-sequence))

;;
;; `compound-apply` is the only place in the interpreter where the
;; `env` register is ever assigned a new value. Just as in the
;; metacircular evaluator, the new environment is constructed from the
;; environment carried by the procedure, together with the argument
;; list and the corresponding list of variables to be bound.
;;

;;
;; 5.4.2 Sequence Evaluation and Tail Recursion
;;
