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

(define (unless-exceptional-value exp)
  (cDddr exp))
