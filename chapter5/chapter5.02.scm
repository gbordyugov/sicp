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

;;
;; exercise 5.7 I don't have the simulator yet
;; 