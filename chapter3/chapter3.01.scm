;;
;; starting Chapter 3, hurrah!
;;

;;
;; exercise 3.1
;;

(define (make-accumulator value)
  (lambda (to-add)
    (set! value (+ value to-add))
    value))

(define acc1 (make-accumulator 10))

(acc1 3)

(acc1 5)
