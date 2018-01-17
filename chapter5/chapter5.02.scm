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

;;
;; exercise 5.11 b)
;;

;;
;; make-save
;;
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      ;; pass the register along with its contents
      (push stack (get-contents reg) reg)
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      ;; pass the register to pop
      (set-contents! reg (pop stack reg))
      (advance-pc))))


(define (make-stack)
  ;; we push pairs (register value)
  (define (register+value reg val)
    (cons reg val))
  ;; accessor
  (define (reg x)
    (car x))
  ;; accessor
  (define (val x)
    (cdr x))
  (let ((s '()))
    (define (push x register)
      (let ((to-push (register+value register x)))
        (set! s (cons to-push s))))
    (define (pop)
      (lambda (register)
        (if (null? s)
          (error "Empty stack: POP")
          (let ((top (car s)))
            (let ((value (val top))
                  (rgstr (reg top)))
              (if (eq? rgstr register)
                (begin
                  (set! s (cdr s))
                  top)
                (error "restoring a different register: POP")))))))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push) ;; returns a procedure
            ((eq? message  'pop)  (pop))
            ((eq? message 'initialize) (initialize))
            (else
              (error "Unknown request: STACK" message))))
    dispatch))

(define (pop stack reg) ((stack 'pop) reg))
(define (push stack value register) ((stack 'push) value register))


;;
;; exercise 5.11 (c)
;;

;;
;; changes to be made:
;;
;; 1. keep a separate stack for each register in
;;    make-new-machine.register-table
;; 2. make push/pop use the corresponding register stack
;; 3. update initialize-stack to initialize stacks of all registers
;;

;;
;; 1.
;;
(define (make-new-machine)
  (let ((pc    (make-register  'pc))
        (flag  (make-register 'flag))
        ;; no global stack anymore
        ;; (stack (make-stack))
        (the-instruction-sequence '()))
    (let* ((the-ops
             ;; those are the supported operations (such ass
             ;; add/sub/mul/div)
             (list (list 'initialize-stack
                         ;; go through all registers and initialize
                         ;; their stacks
                         (lambda ()
                           (for-each (lambda (x)
                                       (let ((stack (caddr x)))
                                         (stack 'initialize)))
                                     register-table)))))
           (register-table
             (list (list 'pc   pc   (make-stack))
                   (list 'flag flag (make-stack)))))
      ;;
      ;; add new register
      ;;
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table (cons (list name
                                           (make-register name)
                                           ;; this is new
                                           (make-stack))
                                     register-table)))
        'register-allocated)
      ;;
      ;; lookup register
      ;;
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (lookup-register-stack name)
        (let ((val (assoc name register-table)))
          (if val
            (caddr val)
            (error "Unknown register: " name))))
      ;;
      ;; run simulation
      ;;
      (define (execute)
        (let ((insts (get-contents pc))) ;; those are the instructions
          (if (null? insts)
            'done
            (begin
              ;; note that his one is a function call with no
              ;; arguments (aka execution procedure). By convention,
              ;; execution procedure must advance programme counter pc
              ((instruction-execution-proc (car insts)))
              (execute)))))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'get-register-stack) lookup-register-stack)
              ;; this one adds new ops to the existing ones
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              (else
                (error "Unknown request: MACHINE" message))))
      dispatch)))

(define (get-register-stack machine reg-name)
  ((machine 'get-register-stack) reg-name))


;;
;; 2.
;;
(define (make-save inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register       machine reg-name))
         (stk (get-register-stack machine reg-name)))
    (lambda ()
      (push stk (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let* ((reg-name (stack-inst-reg-name inst))
         (reg (get-register       machine reg-name))
         (stk (get-register-stack machine reg-name)))
    (lambda ()
      (set-contents! reg (pop stk))
      (advance-pc))))


;;
;; exercise 5.12
;;

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      ;; that's where we analyse instructions
      (make-statistics insts labels machine)
      (update-insts! insts labels machine)
      insts)))
