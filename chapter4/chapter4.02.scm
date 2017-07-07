;;
;; 4.2 Variations on a Scheme - Lazy Evaluation
;;

;;
;; exercise 4.25
;;

;;
;; this would result in an infinite loop
;;


;;
;; exercise 4.26
;;

(define (unless? exp)
  (tagged-list exp 'unless))

(define (unless-condition exp)
  (cadr exp))

(define (unless-condition-usual-value exp)
  (caddr exp))

(define (unless-condition-exceptional-value exp)
  (cdddr exp))

(define (eval-unless exp env)
  (if (true? (eval (unless-condition exp) env))
    (eval (unless-condition-exceptional-value  exp) env)
    (eval (unless-condition-usual-value exp) env)))

;;
;; there are probably some exotic uses of unless as a higher-order
;; procedure, cannot think of them now
;;


;;
;; 4.2.2 An Interpreter with Lazy Evalution
;;
