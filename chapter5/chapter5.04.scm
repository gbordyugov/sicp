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

;;
;; The portion of the explicit-control evaluator at `ev-sequence` is
;; analogous to the metacircular evaluator's eval-sequence procedure.
;; It handles sequences of expressions in procedure bodies or in
;; explicit `begin` expressions.
;;
;; Explicit `begin` expressions are evaluated by placing the sequence
;; of expressions to be evaluated in `unev`, saving `continue` on the
;; stack, and jumping to ev-sequence
;;

ev-begin
  (assign unev (op begin-actions) (reg exp))
  (save continue)
  (goto (label ev-sequence))

;;
;; The implicit sequences in procedure bodies are handled by jumping
;; to `ev-sequence` from `compound-apply`, at which point `continue is
;; already on the stack, having been saved at `ev-application`.
;;
;; The entries at `ev-sequence` and `ev-sequence-continue` form a loop
;; that successively evaluates each expression in a sequence. The list
;; of unevaluated expressions is kept in `unev`. Before evaluating
;; each expression, we check to see if there are additional
;; expressions to be evaluated in the sequence. If so, we save the
;; rest of the unevaluated expressions (hed in `unev`) and the
;; environment in which these must be evaluated (held in `env`) and
;; call eval-dispatch to evaluate the expression. The two saved
;; registers are restored upon the return from this evaluation, at
;; `ev-sequence-continue`.
;;
;; The final expression in the sequence is handled differently, at the
;; entry point `ev-sequence-last-exp`. Since there are no more
;; expressions to be evaluated after this one, we need not save `unev`
;; or `env` before going to `eval-dispatch`. The value of the whole
;; sequence is the value of the last expression, so after the
;; evaluation of the last expression there is nothing left to do
;; excpet contineue at the entry point currently held on the stack
;; (which was saved by `ev-application` or `ev-begin`). Rather than
;; setting up `continue` to arrange for `eval-dispatch` to return here
;; and the restoring `continue` from the stack and continuing at the
;; entry point, we restore `continue` from the stack before going to
;; `eval-dispatch`, so that `eval-dispatch` will continue at that
;; entry point after evaluating the expression.
;;

ev-sequence
  (assign exp (op first-exp) (reg unev))
  (test (op last-exp?) (reg unev))
  (branch (label ev-sequence-last-exp))
  (save unev)
  (save env)
  (assign continue (label eq-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

;;
;; the last expression is handled differently. There are no more
;; expression to be evaluated after this one, we thus need not save
;; unev or env before going to eval-dispatch. The value of the
;; sequence will be the value of the last expression, so after the
;; evaluation of the last expression there is nothing left to do
;; except to continue at the entry point currently held on the stack.
;; Rather than setting up continue to arrange for eval-dispatch to
;; return here and then restoring continue from the stack and
;; continuing at that entry point, we restore continue from the stack
;; before going to eval-dispatch, so that eval-dispatch will continue
;; at that entry point after evaluating the (last) expresssion.
;;
ev-sequence-last-exp
  (restore continue)
  (goto (label eval-dispatch))

;;
;; Tail recursion
;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

;;
;; this is an iterative process. Even though the procedure is
;; syntactically recursive (defined in termsof itself), it is not
;; logically necessary for an evaluator to save information in passing
;; from one call to `sqrt-iter` to the next. An evaluator that can
;; execute a procedure such as `sqrt-iter` without requiring
;; increasing storage (stack) as the procedure continues to call
;; itself is called a tail-recursive evaluator. The metacircular
;; interpretator inherited the tail recursive property of the
;; underlying Scheme implementation.
;;

;;
;; a non-tail-recursive version of ev-sequence
;;

ev-sequence
  (test (op no-more-exps?) (reg unev))
  (branch (label ev-sequence-end))
  (assign exp (op first-exp) (reg unev))
  (save unev)
  (save env)
  (assign continue (label ev-sequence-continue))
  (goto (label eval-dispatch))

ev-sequence-continue
  (restore env)
  (restore unev)
  (assign unev (op rest-exps) (reg unev))
  (goto (label ev-sequence))

ev-sequence-end
  (restore continue)
  (goto (reg continue))

;;
;; this implementation grows the stack on every recursive call
;;

;;
;; 5.4.3 Conditionals, Assignments, and Definitions
;;

;;
;; Special forms are handled by selectively evaluating fragments of
;; the expression (contrarily to function calls, where all arguments
;; are always evaluated).
;;

;;
;; Before evaluating the predicate, we save the if expression itself
;; so that we can later extract the consequent or alternative. We also
;; save the environment, which we will need later in order to evaluate
;; the consequent or the alternative, and we save continue, which we
;; will need later in order to return to the evaluation of the
;; expression that is waiting for the value of the if.
;;

ev-if
  (save exp)
  (save env)
  (save continue)
  (assign continue (label ev-if-decide))
  (assign exp (of if-predicate) (reg exp))
  (goto (label eval-dispatch))

;;
;; When we return from evaluating the predicate, we test whether it
;; was true or false and, depending on the result, place either the
;; consequent or the alternative in exp before going to eval-dispatch.
;; Notice that restoring env and continue here sets up eval-dispatch
;; to have the correct environment and to continue at the right place
;; to receive the value of the if expression.
;;

ev-if-decide
  (restore continue)
  (restore env)
  (restore exp)
  (test (op true?) (reg val))
  (branch (label ev-if-consequent))

ev-if-alternative
  (assign exp (op if-alternative) (reg exp))
  (goto (label eval-dispatch))

ev-if-consequent
  (assign exp (op if-consequent) (reg exp))
  (goto (label eval-dispatch))


;;
;; Assigments and definitions
;;

ev-assignment
  (assign unev (op assignment-variable) (reg exp))
  (save unev)
  (assign exp (op assignment-value) (reg exp))
  (save env)
  (save continue)
  (assign continue (label ev-assignment-1))

ev-assignment-1
  (restore continue)
  (restore env)
  (restore unev)
  (perform (op set-variable-value!) (reg unev) (reg val) (reg env))
  (assign val (const ok))
  (goto (reg continue))
