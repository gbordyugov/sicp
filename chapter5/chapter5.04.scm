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
