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

(define (make-statistics insts labels machine)
  (make-instruction-list  insts labels machine '())
  (make-entry-points-list insts labels machine '())
  (make-stack-list        insts labels machine '())
  (make-source-list       insts labels machine '()))

;;
;; list of all instructions, with duplicates removed, sorted by
;; instruction type
;;
(define (make-instruction-list insts labels machine acc)
  (define (compa a b)
    (cond ((and (symbol? a) (symbol? b)) (symbol<? a b))
          ((symbol? a) (compa a (car b)))
          ((symbol? b) (compa (car a) b))
          (else (compa (car a) (car b)))))
  (if (null? insts)
    (sort acc compa)
    (let ((inst (car insts)))
      (let ((inst-type (car inst)))
        (if (assoc inst acc)
          (make-instruction-list (cdr insts) labels machine acc)
          (make-instruction-list (cdr insts) lables machine (cons inst acc)))))))

(define (make-set lst)
  (define (iter items set)
    (if (null? items)
      set
      (let ((ele (car items)))
        (if (member ele set)
          (iter (cdr items) (cons (ele set)))
          (iter (cdr items)            set)))))
  (iter lst '()))

;;
;; list, without duplicates, of the registers tohold entry points,
;; i.e.  those referenced by goto instructions.
;;
(define (make-entry-points-list insts labels machine acc)
  (if (null? insts)
    ;; TODO: make set of it
    (make-set acc)
    (let ((inst (car insts)))
      (let ((inst-type (car inst))
            (inst-body (cdr inst)))
        (if (eq? 'goto inst-type)
          (let ((label (cadr body)))
            (make-entry-points-list (cdr insts) labels machine (cons label acc)))
          (make-entry-points-list (cdr insts) labels machine acc))))))

;;
;; list (without duplicates) of the registers that are saved or
;; restored
;;
(define (make-stack-list insts labels machine acc)
  (if (null? insts)
    ;; TODO: make set of it
    (make-set acc)
    (let ((inst (car insts)))
      (let ((inst-type (car inst))
            (inst-body (cdr inst)))
        (if (or (eq? 'save inst-type)
                (eq? 'restore inst-type))
          (let ((what (car inst-body)))
            (make-entry-points-list (cdr insts) labels machine (cons what acc)))
          (make-entry-points-list (cdr insts) labels machine acc))))))

;;
;; exercise 5.13
;;

(define (collect-register-names instructions)
  (define (extract-register-name operand)
    (if (and (pair? operand)
             (eq? (car operand) 'reg))
      (cadr operand)
      '()))
  (define (iter insts registers)
    (if (null? insts)
      registers
      (let* ((inst (car insts))
             (operands (cdr inst))
             (reg-names (filter (lambda (x) (not (null? x)))
                                (extract-register-name operands))))
        (collect-register-names (cdr insts)
                                (append reg-names registers)))))
  (iter instructions '()))

;;
;; installing them into the machine is trivial
;;

;;
;; exercise 5.14
;;

;;
;; I'll here repeat the factorial controller from the text
;;

(load "machine.scm")
(define fact-machine
  (make-machine
    '(continue n val)
    (list (list '- -)
          (list '* *)
          (list '= =))
    '((assign continue (label fact-done))
      fact-loop
      (test (op = ) (reg n) (const 1))
      (branch (label base-case))
      ;;
      ;; set up for the recursive call by saving n and continue.
      ;; st up ocontinue so that the computation will continue
      ;; at after-fact when the subroutine returns.
      (save continue)
      (save n)
      (assign n (op -) (reg n) (const 1))
      (assign continue (label after-fact))
      (goto (label fact-loop))
      after-fact
      (restore n)
      (restore continue)
      (assign val (op *) (reg n) (reg val))  ; val now contains n(n-1)!
      (goto (reg continue))                  ; return to caller
      base-case
      (assign val (const 1))                 ; base case: 1! = 1
      (goto (reg continue))                  ; return to caller
      fact-done)))

(set-register-contents! fact-machine 'n 5)
(start fact-machine)
(get-register-contents fact-machine 'val)

((fact-machine 'stack) 'print-statistics)

;;
;; exercise 5.15
;;
;; see machine-exercise-5.15.scm
;;


;;
;; exercise 5.16
;;
;; see machine-exercise-5.16.scm
;;

;;
;; exercise 5.17
;;
;; see machine-exercise-5.17.scm
;;

;;
;; some playing around
;;

;;
;; this is how extract-labels is used
;;
(define (assemble controller-text machine)
  (extract-labels
    controller-text
    (lambda (insts labels)
      (update-insts! insts labels machine)
      insts)))



(define (make-instruction-with-label text label)
  (list text label '()))
(define (make-label-entry label-name insts)
  (list 'label-name label-name 'instructions insts))
(define (make-instruction text)
  (list text '() '()))

(define (print-insts-and-labels insts labels label)
  (newline)
  (display (list 'insts '= insts))
  (newline)
  (display (list 'labels '= labels)))
(define (check-extractor controller-text)
  (extract-labels
    controller-text
    print-insts-and-labels))
(define (extract-labels text receive)
  (define label? symbol?)
  (if (null? text)
    (receive '() '() '())
    (extract-labels
      (cdr text)
      (lambda (insts labels prev-label)
        (let ((next-inst (car text)))
          (if (label? next-inst)
            (let ((label next-inst))
              (receive insts (cons (make-label-entry next-inst insts) labels) label))
            (let ((label '()))
              (receive (cons (make-instruction-with-label next-inst prev-label) insts) labels label))))))))

;;
;; new version without this tricky recursion
;; now extract-labels returns a cons with car beingin the labels
;; and cdr being the instructions
;;

(define (assemble controller-text machine)
  (let* ((res (extract-labels controller-text))
         (insts  (car res))
         (labels (cdr res)))
    (update-insts! insts labels machine)
    insts))

(define (extract-labels text)
  (if (null? text)
    (cons '() '())
    (let* ((result (extract-labels (cdr text)))
           (insts     (car result))
           (labels    (cdr result))
           (next-inst (car text)))
      (if (symbol? next-inst) ;; is it a label?
        ;; extend labels list
        (cons insts
              (cons (make-label-entry next-inst insts)
                    labels))
        ;; extend instruction list
        (cons (cons (make-instruction next-inst) insts)
              labels)))))

;;
;; those ones are needed to keep the information about labeling
;;
(define (make-instruction text)
  (list text '() '()))

(define (make-instruction-with-label text label)
  (list text label '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-label inst)
  (cadr inst))

(define (instruction-execution-proc inst)
  (caddr inst))

(define (set-instruction-label! inst label)
  (set-car! (cdr inst) proc))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cddr inst) proc))

;;
;; this one puts labels in instructions relying on the data structures
;; above
;;
(define (extract-labels text)
  (if (null? text)
    (cons '() '())
    (let* ((result (extract-labels (cdr text)))
           (insts     (car result))
           (labels    (cdr result))
           (next-inst (car text)))
      (if (symbol? next-inst) ;; is it a label?
        ;; extend labels list
        (let* ((instruction (if (null? insts)
                              '()
                              ;; take instruction text (car) of the
                              ;; first instruction
                              (car (car insts))))
               (rest-insts  (cdr insts))
               (label        next-inst)
               (instruction-with-label
                 (make-instruction-with-label instruction label)))
          (cons (cons instruction-with-label rest-insts)
                (cons (make-label-entry next-inst insts)
                      labels)))
        ;; extend instruction list
        (cons (cons (make-instruction next-inst) insts)
              labels)))))

(define text
  '((nothing)
    label-1
    (what?)
    (assign bla (op +) (reg b) (reg c))
    label-2
    (assign blu (op /) (reg d) (reg e))))

;; (extract-labels text)

;; (check-extractor text)


;;
;; exercise 5.18
;;

(define (make-register name)
  (let ((contents '*unassigned*)
        (trace-on #f))
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'trace-on)  (set! trace-on #t))
            ((eq? message 'trace-off) (set! trace-on #f))
            ((eq? message 'set)
             (lambda (value)
               (if trace-on
                 (newline)
                 (display 'old-value contents 'new-value value))
               (set! contents value)))
            (else
              (error "Unknown request: REGISTER" message))))
  dispatch))

(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))
(define (register-trace-on register)
  (register 'trace-on))
(define (register-trace-off register)
  (register 'trace-off))

;;
;; extending the interface of the machine is trivial
;;


;;
;; exercise 5.19
;;

;;
;; first off, changes in the instruction structure:
;;
;; instruction now is:
;; - instruction text
;; - execution procedure
;; - label context

;;
;; constructor
;;
(define (make-instruction text)
  (list text '() (make-label-context)))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cadr inst))

(define (instruction-label-context inst)
  (caddr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-car! (cdr inst) proc))

;;
;; home-grown hash tables, basically from Chapter 3.3.3 Representing
;; Table
;;

(define (make-label-context)
  (cons '*label-context* '()))
(define (lc-put-label-with-offset context label offset)
  (let* ((table (cdr context))
         (value (assoc label table)))
    (if value
      (set-cdr! value offset)
      (set-cdr! context (cons (cons label offset) table)))))
(define (lc-get-offset context label)
  (let ((table (cdr context)))
    (cdr (assoc label table))))

;;
;; old version of extract-labels
;;
(define (extract-labels text)
  (if (null? text)
    (cons '() '())
    (let* ((result (extract-labels (cdr text)))
           (insts     (car result))
           (labels    (cdr result))
           (next-inst (car text)))
      (if (symbol? next-inst) ;; is it a label?
        ;; extend labels list
        (cons insts
              (cons (make-label-entry next-inst insts)
                    labels))
        ;; extend instruction list
        (cons (cons (make-instruction next-inst) insts)
              labels)))))
