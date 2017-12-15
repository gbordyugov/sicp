;;
;; Simulator of register machines of Chapter 5.2
;;

;;
;; machines are procedures with local state, able to accept messages
;;
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each
      (lambda (register-name)
        ((machine 'allocate-register) register-name))
      register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
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


;;
;; The stack
;;

;;
;; This stack is also a procedure with local state
;;

(define (make-stack)
  (let ((s '()))
    (define (push x) (set! s (cons x s)))
    (define (pop)
      (if (null? s)
        (error "Empty stack: POP")
        (let ((top (car s)))
          (set! s (cdr s))
          top)))
    (define (initialize)
      (set! s '())
      'done)
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message  'pop)  (pop))
            ((eq? message 'initialize) (initialize))
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
        (flag  (make-register) 'flag)
        (stack (make-stack))
        (the-instruction-sequence '()))
    (let ((the-ops
            (list (list 'initialize-stack
                        (lambda () (stack 'initialize)))))
          (register-table
            (list (list 'pc pc)
                  (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
          (error "Multiply defined register: " name)
          (set! register-table (cons (list name (make-register name))
                                     register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
            (cadr val)
            (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc))) ;; those are the instructions
          (if (null? insts)
            'done
            (begin ((instruction-execution-proc (car insts))) ;; this would modify pc
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
              ((eq? message 'install-operations)
               (lambda (ops)
                 (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
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
                 vale)
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
  (extract-lables
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
        (set-instruction-execution-proc!
          inst
          (make-execution-procedure
            (instruction-text inst)
            labels machine pf flag stack ops)))
      insts)))

;;
;; abstractions for instructions
;; for each instruction, it's the instruction text together with the
;; corresponding execution procedure (is not available at the moment
;; when extract-labels is called, and updated later by update-insts!.
;;

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
;; procedure) for each instruction. During a simulation run, those
;; procedures (aka execution procedures below) get called in the
;; specified order to move things around
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
        (else
          (error "Unknown instruction type: ASSEMBLE" inst))))

;;
;; assign
;;

(define (make-assign inst machine labels operations pc)
  (let ((target (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
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

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))
(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;;
;; make-test
;;

(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
      (let ((condition-proc (make-operation-exp condition
                                                machine labels
                                                operations)))
        (lambda ()
          (set-contents! flag (condition-proc))
          (advance-pc pc)))
      (error "Bad TEST instruction: ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-condition))

;;
;; make-branch
;;

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
      (let ((insts (lookup-label labels (label-exp-label dest))))
        (lambda ()
          (if (get-contents flag)
            (set-contents! pc insts)
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

(define (make-save inst machine stack pc)
  (let ((reg (get-register machine (stack-inst-reg-name inst))))
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))
