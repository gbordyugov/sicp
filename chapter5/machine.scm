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
