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
