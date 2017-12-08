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
