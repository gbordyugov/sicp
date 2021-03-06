;;
;; started Chapter 5
;;

;;
;; register machine -- a model for modern computers
;;
;; has a fixed set of storage elements, called registers
;;
;; sequentially executes instructions that manimulate those registers

;;
;; whereas addition and multiplication can be easily implemented in
;; hardware, such operations as car, cdr, and cons need to know more
;; about memory and how to allocate it
;;

;;
;; The remainder machine: data paths controlled by buttons plus
;; controller that pushes the buttons
;;

;;
;; Exercise 5.1
;;

;;
;; Registers: iter, product, counter, a (for mul), b (for add), n
;;
;; Operations: mul, add
;;
;; obvious set of buttons
;;
;; decision for (> counter n)
;;

;;
;; 5.1.1 A Language for Describing Register Machines
;;

;;
;; data paths are described by registers and operations.
;;
;; register: name + set of buttons that control assignment to it
;;
;; button: name + source of data that enters the register under the
;; buttons's control (source being a register, a constant, or an
;; operation)
;;
;; operation: name + inputs (registers or constants)
;;

;;
;; controller: sequence of instructions together with labels that
;; identify entry points
;;
;; Instruction can be either:
;;  - the name of a data-path button to push to assign a value to a
;;    registers
;;  - a test instruction that performs a specified test
;;  - a conditional branch, specified by a controller label, based on
;;    the results of the previous test
;;  - an unconditional branch (goto instruction)
;;

;;
;; A specification of the GCD machine
;;

(data-paths
  (registers
    ((name a)
     (buttons ((name a<-b) (source (register b)))))
    ((name b)
     (buttons ((name b<-t) (source (register t)))))
    ((name t)
     (buttons ((name t<-r) (source (operation rem))))))
  (operations
    ((name rem)
     (inputs (register a) (register b)))
    ((name =)
     (inputs (register b) (constant 0)))))

(controller
  test-b                          ;; label
  (test =)                        ;; test
  (branch (label gcd-done))       ;; conditional branch
  (t<-r)                          ;; button push
  (a<-b)                          ;; button push
  (b<-t)                          ;; button push
  (goto (label test-b))           ;; unconditional branch
  gcd-done)

;;
;; Simplified GCD machine
;;

(controller
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done)

;;
;; exercise 5.2
;;

(controller
  (assign product (const 1))
  (assign counter (const 1))
  test-counter
  (test (op >) (reg counter) (reg n))
  (branch (label factorial-done))
  (assign prod (op *) (reg product) (reg counter))
  (assign counter (op +) (reg counter) (const 1))
  (assign product (reg prod))
  (goto (label test-counter))
  factorial-done)


;;
;; GCD machine with IO
;;

(controller
  gcd-loop
  (assign a (op read))
  (assign b (op read))
  test-b
  (test (op =) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label test-b))
  gcd-done
  (perform (op print) (reg a))
  (goto (label gcd-loop)))

;;
;; 5.1.2 Abstraction in Machine Design
;;

(define (remainder n d)
  (if (< n d)
    n
    (remainder (- n d) d)))

;;
;; GCD with remainder
;;

(controller test-b
            (test (op =) (reg b) (const 0))
            (branch (label gcd-done))
            (assign t (reg a))
            rem-loop
            (test (op <) (reg t) (reg b))
            (branch (label rem-done))
            (assign t (op -) (reg t) (reg b))
            (goto (label rem-loop))
            rem-done
            (assign a (reg b))
            (assign b (reg t))
            (goto (label test-b))
            gcd-done)

;;
;; exercise 5.3
;;

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;;
;; simple version with good-enough? as a primitive
;;
(controller (assign guess (const 1.0))
            test-good-enough
            (test (op good-enough?) (reg guess))
            (branch (label sqrt-done))
            (assign x-by-guess (op /) (reg x) (reg guess))
            (assign sum (op +) (reg guess) (reg x-by-guess))
            (assign guess (op /) (reg sum) (const 2.0))
            (goto (label test-good-enough))
            sqrt-done)

;;
;; more elaborated version with good-enough? coded
;;
(controller (assign guess (const 1.0))
            test-good-enough
            ;; (test (op good-enough?) (reg guess))
            (assign squared-guess (op *) (reg guess) (reg guess))
            (assign squared-guess-minus-x
                    (op -) (reg squared-guess) (reg x))
            (test (op <) (reg squared-guess-minus-x) (const 0.001))
            (branch (label sqrt-done))
            (assign x-by-guess (op /) (reg x) (reg guess))
            (assign sum (op +) (reg guess) (reg x-by-guess))
            (assign guess (op /) (reg sum) (const 2.0))
            (goto (label test-good-enough))
            sqrt-done)

;;
;; 5.1.3 Subroutines
;;


;;
;; Figure 5.8: two nearly identical GCD machines
;;

(controller
  ;; ...
  gcd-1
  (test (op = 1) (reg b) (const 0))
  (branch (label gcd1-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd-1))
  gcd1-done

  ;;...
  gcd-2
  (test (op = 1) (reg b) (const 0))
  (branch (label gcd2-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd-2))
  gcd2-done
  )

;;
;; the only difference is the entry and exit labels
;;

;;
;; first solution: introduce a special register called `continue`,
;; having values 0 or 1, and depending on this register,
;; we will goto either to gcd1-done or gcd2-done
;;

(controller
  ;;
  ;; rest of the code
  ;;
  gcd-done
  (test (op =) (reg continue) (const 0))
  (branch (label gcd1-done))
  (branch (label gcd2-done))
  )

;;
;; next idea: `continue` register should be able to hold a label
;; (a jump address)
;;

(controller
  gcd
  (test (op = ) (reg b) (const 0))
  (branch (label gcd-done))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd))
  gcd-done
  (goto (reg continue))

  ;;
  ;; first call of gcd
  ;;
  (assign continue (label after-gcd-1))
  (goto (label gcd))
  after-gcd-1

  ;;
  ;; second call of gcd
  ;;
  (assign continue (label after-gcd-2))
  (goto (label gcd))
  after-gcd-2
  ;;
  ;; more code
  ;;
  )

;;
;; different subroutines can use different or the same
;; continuation register
;;


;;
;; 5.1.4 Using a Stack to Implement Recursion
;;

;;
;; iterative process: the state of the machine is completely
;; determined by the IP (instruction pointer) and the contents of the
;; registers
;;

;;
;; both GCD and factorial procedures call themselves, but in GCD the
;; recursive call is the last thing, and in factorial not
;;
;; GCD: the answer to the reduced problem is the answer to the
;; original problem
;;
;; Factorial: the answer of the reduced problem is only part of the
;; answer to the original problem
;;
;; tail recursion!
;;
;; in the subproblem, the contents of the registers is different from
;; that in the superproblem

;;
;; need of a stack with two opertions:
;; - save aka push
;; - restore aka pop

(controller
  (assign continue (label fact-done)) ; set up final return address
  fact-loop
  (test (op =) (reg n) (const 1))
  (branch (label base-case))
  ;;
  ;; preparing the recursive call
  ;;
  (save continue)
  (save n)
  (assign n (op -) (reg n) (const 1))
  (assign continue (label after-fact))
  (goto (label fact-loop))
  after-fact
  (restore n)
  (restore continue)
  (assign val (op *) (reg n) (reg val)) ; val now contains n(n-1)!
  (got (reg continue))                  ; return to caller
  base-case
  (assign val (const 1))                ; base case: 1! = 1
  (goto (reg-continue))                 ; return to caller
  fact-done)

;;
;; in the recursive calls, the continue register must always be saved
;;
;; so have all registered that are to be modified during the call
;;

;;
;; A double recursion
;;

;;
;; consider
;;

(define (fib b)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

;;
;; three registers: n, val, and continue
;;

(controller
  (save continue)                    ;; from the previous context
  (assign continue (label fib-done)) ;; where to return after we're done, is somewhere else
  (goto (label fib-loop))            ;; call the function
  fib-loop                           ;; entry point to the subroutine
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; preparing to compute Fib(n-1)
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (goto (label fib-loop))   ;; perform recursive call
  afterfib-n-1              ;; upon return, val contains Fib(n-1)
  (restore n)
  (restore continue)
  ;; set up to compute Fib(n-2)
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val)                ;; save Fib(n-1)
  (goto (label fib-loop))   ;; perform recursive call
  afterfib-n-2
  (aassign n (reg val))     ;; n   <- Fib(n-2)
  (restore val)             ;; val <- Fib(n-1)
  (restore continue)
  (assign val (op +) (reg val) (reg n))
  (goto (reg continue))     ;; return to caller
  immediate-answer
  (assign val (reg n))      ;; base case Fib(n) = n
  (goto (reg continue))
  )

;;
;; exercise 5.4
;;

;;
;; (a)
;;
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(controller
  (save continue)
  (assign continue (label expt-done))      ;; is somewhere else
  (goto (label expt-loop)) ;; the actual call
  expt-loop ;; input in registers b and n, return value in val
  (test (op =) (reg n) (const 0))
  (branch (label base-case))
  ;; preparing recursive call
  (save continue)
  (save n)
  (assign continue (label after-recursion))
  (assign n (op -) (reg n) (const 1))
  (goto (label expt-loop))
  (restore n)
  (restore continue)
  (assign val (op *) (reg b) (reg val))
  (goto (reg continue))
  base-case
  (assign val 1)
  (goto (reg continue)))


;;
;; (b)
;;

(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(controller
  ;; registers: b, n, counter, product, val (for return value)
  (assign counter (reg n))
  (assign product (const 1))
  (assign continue (label expt-done)) ;; is somewhere
  (goto (label expt-iter))
  expt-iter
  (test (op =) (reg counter) (const 0))
  (branch (label base-case))
  (assign counter (op -) (reg counter) (const 1))
  (assign product (op *) (reg b) (reg product))
  (goto (label expt-iter))
  base-case
  (assign val (reg product))
  (goto (reg continue)))


;;
;; exercise 5.5: I hand-simulated it - believe me!
;;

;;
;; exercise 5.6: lines 399 and 402
;;

;;
;; 5.1.5 Instruction sumamry
;;

(assign <register-name> (reg <register-name>))
(assign <register_name> (const <constant-value>))
(assign <register-name>
        (op <operation-name>)
        <input1> ... <inputn>)
(perform (op <operation-name>) <input1> ... <inputn>)
(test (op <operation-name>) <input1> ... <inputn>)
(branch (label <label-name>))
(goto (label <label-name>))

(assign <register-name> (label <label-name>)))
(goto (reg <register-name>))

(save <register-name>)
(restore <register-name>)

(const "abc") ;; strings
(const abc)   ;; symbols
(const (a b c)) ;; lists
(const ())      ;; the empty list
