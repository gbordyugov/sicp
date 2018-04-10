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
