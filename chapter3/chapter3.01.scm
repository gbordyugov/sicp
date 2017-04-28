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

;;
;; exercise 3.2
;;

(define (make-monitored f)
  (let ((no-of-calls 0))
    (begin
      (define (monitored x)
        (if (eq? x 'how-many-calls?)
          no-of-calls
          (begin
            (set! no-of-calls (+ no-of-calls 1))
            (f x))))
      monitored)))

(define s (make-monitored sqrt))

(s 'how-many-calls?)
(s 9)
(s 9)
(s 'how-many-calls?)
(s 9)
(s 16)
(s 'how-many-calls?)
