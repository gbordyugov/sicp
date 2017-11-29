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
;; exercise 5.5
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
  (branch (label after-gcd-1))
  (assign t (op rem) (reg a) (reg b))
  (assign a (reg b))
  (assign b (reg t))
  (goto (label gcd-1))

  ;;...
  gcd-2
  ;; basically the same code as above
  )

