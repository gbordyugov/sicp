;;
;; Simulator of register machines of Chapter 5.2
;; extended by instruction counter, as for exercise 5.15
;; extended by instruction tracing, as for exercise 5.16
;; extended by label printing, as for exercise 5.17
;;

;;
;; machines are procedures with local state, able to accept messages
;;

(define (make-machine register-names ops controller-text)
  ;; make-new-machine defined below
  (let ((machine (make-new-machine)))
    ;; allocate registers
    (for-each
      (lambda (register-name)
        ((machine 'allocate-register) register-name))
      register-names)
    ;; install operations
    ((machine 'install-operations) ops)
    ;; assemble and install instruction sequence
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    ;; return the newly created machine
    machine))

;;
;; Registers
;;

;;
;; registers are procedures with local state
;;

(define (make-register name)
  (let ((contents '*unassigned*))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value)
               (set! contents value)))
            (else
              (error "Unknown request: REGISTER" message))))
  dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))


;;
;; The stack
;;

;;
;; This stack is also a procedure with local state
;;

(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth     0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
        (error "Empty stack: POP")
        (let ((top (car s)))
          (set! s (cdr s))
          (set! current-depth (- current-depth 1))
          top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth     0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes  '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push) ;; returns a procedure
            ((eq? message  'pop)  (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
              (error "Unknown request: STACK" message))))
  dispatch))

(define (pop stack) (stack 'pop))
(define (push stack value) ((stack 'push) value))


;;
;; the basic machine
;;

;;
;; The `pc` register points to the instruction to be executed next
;;

(define (make-new-machine)
  (let ((pc    (make-register  'pc))
        (flag  (make-register 'flag))
        (stack (make-stack))
        ;; whether to trace instructions
        (instruction-tracing #f)
        ;; self-explanatory, I hope
        (instruction-counter 0)
        ;; this one is going to be a list of pairs with first element
        ;; being a lambda of no arguments - an instruction procedure
        ;; and the second one being convenience information (source
        ;; code/line no, etc)
        (the-instruction-sequence '()))
    (let ((the-ops
            ;; those are the supported operations (such ass
            ;; add/sub/mul/div)
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc)
                  (list 'flag flag))))
      ;;
      ;; add new register
      ;;
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table (cons (list name (make-register name))
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
              (if instruction-tracing
                (begin
                  (newline)
                  (display (instruction-text (car insts))))
                '())
              ((instruction-execution-proc (car insts)))
              (set! instruction-counter (+ 1 instruction-counter))
              (execute)))))
      (define (trace-on)
        (set! instruction-tracing #t))
      (define (trace-off)
        (set! instruction-tracing #f))
      (define (reset-instruction-counter)
        (set! instruction-counter 0))
      (define (print-instruction-counter)
        (newline)
        (display (list 'instruction-counter '= instruction-counter)))
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq)
                 (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ;; this one adds new ops to the existing ones
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;; instruction counter stuff
              ((eq? message 'print-instruction-counter)
               (print-instruction-counter))
              ((eq? message 'reset-instruction-counter)
               (reset-instruction-counter))
              ;; turn tracing on and off
              ((eq? message 'trace-on)  (trace-on))
              ((eq? message 'trace-off) (trace-off))
              (else
                (error "Unknown request: MACHINE" message))))
      dispatch)))

;;
;; the interface
;;

(define (start machine) (machine 'start))
(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))
(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name)
                 value)
  'done)
(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;;
;; 5.2.2 The Assembler
;;

;;
;; The assembler transforms the sequence of controller expressions for
;; a machine into a corresponding list of machine instructions, each
;; with its execution procedure
;;

;;
;; an execution procedure is a lambda of no arguments that performs
;; the action, encoded by the corresponding controller expression
;;
;; by convention, it is job of execution procedures to advance the
;; programme counter `pc`
;;

;;
;; Before generating instructions, the assembler must scan the labels
;; in order to find out the addresses of jumps
;;
;; those executions procedures seem to be really important!
;;

;;
;; note that the assember is parameterized by both the controller AND
;; the machine
;;

(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))

;;
;; extract-labels takes as argument the text of the controller plus
;; the receiver procedure, which is called with two values: list of
;; instructions `insts` and table `labels`, whicha associates each
;; label from text with the position in the list `insts` that the
;; label designates.

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
            (receive insts
                     (cons (make-label-entry next-inst insts) labels))
            (receive (cons (make-instruction next-inst) insts)
                     labels)))))))

;;
;; update-insts!
;;

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
      (lambda (inst)
        ;; initially, there is no execution procedure in those
        ;; instructions. This loop fills out those missing execution
        ;; procedures (which are lambdas of no arguments)
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pc flag stack ops)))
      insts)))

;;
;; abstractions for instructions
;; for each instruction, it's the instruction text together with the
;; corresponding execution procedure (is not available at the moment
;; when extract-labels is called, and updated later by update-insts!).
;;

;; in the beginning there is no execution procedure
(define (make-instruction text) (cons text '()))
(define (instruction-text inst) (car inst))
(define (instruction-execution-proc inst) (cdr inst))
(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

;;
;; labels and label look-up code
;;
(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
      (cdr val)
      (error "Undefined label: ASSEMBLE" label-name))))

;;
;; exercise 5.8 skipped: I don't have the machine still
;;


;;
;; 5.2.3 Generating Execution Procedures for Instructions
;;
;; the idea is to dynamically create a procedure (a real Scheme
;; procedure with no arguments) for each instruction. During a
;; simulation run, those procedures (aka execution procedures below)
;; get called in the specified order to move things around
;;

;;
;; here, argument `inst` is not a whole instruction in the above sense
;; (text + lambda), buth just text
;;
(define (make-execution-procedure
          inst labels machine pc flag stack ops)
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        ((eq? (car inst) 'nop)
         (make-nop pc))
        (else
          (error "Unknown instruction type: ASSEMBLE" inst))))

;;
;; assign
;;

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    ;; value-proc is a procedure of no arguments that will be called
    ;; from within the execution procedure below
    (let ((value-proc
            ;; is the value a result of an operation (such as
            ;; add/sub/div/mul, etc?
            (if (operation-exp? value-exp)
              ;; yes, make an operation expression
              ;; see below for the definition of the procedure
              (make-operation-exp value-exp
                                  machine labels operations)
              ;; no make a primitive expression
              ;; see below for the definition of the procedure
              (make-primitive-exp (car value-exp) machine labels))))
      (lambda () ;; execution procedure for assign
        (set-contents! target (value-proc))
        (advance-pc pc)))))

;;
;; Example:
;; (assign counter (op -) (reg counter) (const 1))
;;
(define (assign-reg-name assign-instruction)
  (cadr assign-instruction)) ;; `counter` in the example before
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction)) ;; everything starting with `(op -)`

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;;
;; make-test
;;

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      ;; condition-proc is a procedure that will be called from the
      ;; execution procedure (the lambda ()) below
      (let ((condition-proc (make-operation-exp condition
                                                machine labels
                                                operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

;;
;; make-branch
;;

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts (lookup-label labels (label-exp-label dest))))
        (lambda ()
          ;; if flag is set, ...
          (if (get-contents flag)
            ;; ... jump,
            (set-contents! pc insts)
            ;; ... otherwise just step further
            (advance-pc pc))))
        (error "Bad BRANCH instruction: ASSEMBLE" inst))))
(define (branch-dest branch-instruction)
  (cadr branch-instruction))

;;
;; make-goto
;;

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp?  dest)
           (let ((insts (lookup-label labels (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg (get-register machine (register-exp-reg dest))))
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction: ASSEMBLE" inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;;
;; Other instructions
;;

;;
;; make-save
;;
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

;;
;; make-restore
;;
(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

;;
;; make-perform
;;
(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
      (let ((action-proc (make-operation-exp action
                                             machine
                                             labels
                                             operations)))
        (lambda ()
          (action-proc)
          (advance-pc pc)))
      (error "Bad PERFORM instruction: ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;;
;; to exercise 5.10
;;

(define (make-nop pc)
  (lambda ()
    (advance-pc pc)))
;;
;; execution procedures for subexpressions
;;

(define (make-primitive-exp exp machine labels)
  (cond
    ;; if it's a constant, just make a lambda that returns that
    ;; constant
    ((constant-exp? exp)
     (let ((c (constant-exp-value exp)))
       (lambda () c)))
    ;; if it's a label, look it up and return it by a lambda
    ((label-exp? exp)
     (let ((insts (lookup-label labels
                                (label-exp-label exp))))
       (lambda () insts)))
    ;; if it's a register, fetch the registers and in the lambda,
    ;; return its contents
    ((register-exp? exp)
     (let ((r (get-register machine (register-exp-reg exp))))
       (lambda () (get-contents r))))
        (else
          (error "Unknown expression type: ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

;;
;; those are used for operations and make use of the operation table
;; of the machine
;;

;;
;; it is assumed that the first element of exp is an operation, like
;; `(op -)` and the rest elements are the arguments to that operation
;;

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp)
                         operations))
        (aprocs
          (map (lambda (e)
                 (make-primitive-exp e machine labels))
               ;; exercise 5.9
               (filter (lambda (x)
                         (not (label-exp? x)))
                       (operation-exp-operands exp)))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))
(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))
(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
      (cadr val)
      (error "Unknown operation: ASSEMBLE" symbol))))

;;
;; a little helper function
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))
