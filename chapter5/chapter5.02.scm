;;
;; 5.2. A Register-Machine Simulator
;;


;;
;; (make-machine <register-names> <operations> <controller>)
;; constructs a model of the machine
;;
;; (set-register-contents! <machine-model> <register-name> <value>)
;; does what it's supposed to do
;;
;; (get-register-contents <machine-model> <register-name>)
;; dito
;;
;; (start <machine-model>)
;; simulates the execution of the given machine
;;

;;
;; an example:
;;

(load "machine.scm")
(define gcd-machine
  (make-machine
    '(a b t)
    (list (list 'rem remainder) (list '= =)) ;; note the quotes
    '(test-b (test (op =) (reg b) (const 0))
             (branch (label gcd-done))
             (assign t (op rem) (reg a) (reg b))
             (assign a (reg b))
             (assign b (reg t))
             (goto (label test-b))
             gcd-done)))

(set-register-contents! gcd-machine 'a 206)

(set-register-contents! gcd-machine 'b 40)

(start gcd-machine)

(get-register-contents gcd-machine 'a)

;;
;; exercise 5.7
;;

;;
;; the code
;;
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1) (* b product))))
  (expt-iter n 1))

(load "machine.scm")
(define expt-machine
  (make-machine
    ;; registers
    '(b n counter product val continue)
    ;; operations
    (list (list 'rem remainder)
          (list '=   =)
          (list '*   *)
          (list '-   -))
    ;; registers: b, n, counter, product, val (for return value)
    '(begin (assign counter (reg n))
            (assign product (const 1))
            expt-iter
            (test (op =) (reg counter) (const 0))
            (branch (label base-case))
            (assign counter (op -) (reg counter) (const 1))
            (assign product (op *) (reg b) (reg product))
            (goto (label expt-iter))
            base-case
            (assign val (reg product))
            expt-done)))

(set-register-contents! expt-machine 'b 2)
(set-register-contents! expt-machine 'n 20)
(start expt-machine)

(get-register-contents expt-machine 'product)


;;
;; exercise 5.8
;;
(define (extract-labels text receive)
  (if (null? text)
    (receive '() '())
    (extract-labels
      (cdr text)
      ;; we "update" receive by wrapping it in a lambda that updates
      ;; the lists of instructions and labels depending on (car text)
      (lambda (insts labels)
        (let ((next-inst (car text)))
          (if (symbol? next-inst)   ;; is a label?
            (if (assoc next-inst labels)
              (error "ASSEMBLY: label already defined" next-inst)
              (receive insts
                       (cons (make-label-entry next-inst insts) labels)))
            (receive (cons (make-instruction next-inst) insts)
                     labels)))))))

;;
;; exercise 5.9: see machine.scm
;;

;;
;; exercise 5.10: see machine.scm for `make-nop` ;-)
;;

;;
;; Happy New Year!
;;

;;
;; exercise 5.11 a)
;;

;;
;; we can directly (restore n) to obtain Fib(n-1) and add it to (reg
;; val) to obtain the sum
;;
