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
;; exercise 5.7 I don't have the simulator yet
;; 

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
