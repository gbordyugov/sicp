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


;;
;; exercise 3.3
;;

(define (make-account password balance)
  (define (withdraw pass amount)
    (if (not (eq? pass password))
      "wrong password in withdraw"
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds")))
  (define (deposit pass amount)
    (if (not (eq? pass password))
      "wrong password in withdraw"
      (begin
        (set! balance (+ balance amount))
        balance)))
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit)  deposit)
          (else (error "Unknown request: MAKE-ACCOUNT" m))))
  dispatch)

(define acc (make-account 'pass 10))

((acc 'deposit) 'pass 10)

((acc 'deposit) 'no-pass 10)

((acc 'withdraw) 'pass 10)

((acc 'withdraw) 'no-pass 10)

;;
;; exercise 3.4
;;

(define (make-account password balance)
  (let ((no-of-wrong-attempts 0))
    (begin
      ;;
      ;;
      ;;
      (define (call-the-cops msg)
        (display msg)
        (newline))
      ;;
      ;;
      ;;
      (define (withdraw pass amount)
        (if (not (eq? pass password))
          (begin
            (set! no-of-wrong-attempts (+ no-of-wrong-attempts 1))
            (if (> no-of-wrong-attempts 6)
              (call-the-cops "ALARM!"))
            "wrong password in withdraw")
          (if (>= balance amount)
            (begin
              (set! no-of-wrong-attempts 0)
              (set! balance (- balance amount))
              balance)
            "Insufficient funds")))
      ;;
      ;;
      ;;
      (define (deposit pass amount)
        (if (not (eq? pass password))
          (begin
            (set! no-of-wrong-attempts (+ no-of-wrong-attempts 1))
            (if (> no-of-wrong-attempts 6)
              (call-the-cops "ALARM!"))
            "wrong password in withdraw")
          (begin
            (set! no-of-wrong-attempts 0)
            (set! balance (+ balance amount))
            balance)))
      ;;
      ;;
      ;;
      (define (dispatch m)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit)  deposit)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      ;;
      ;;
      ;;
      dispatch)))

(define acc (make-account 'pass 10))

((acc 'deposit) 'pass 10)

((acc 'deposit) 'no-pass 10)

((acc 'withdraw) 'pass 10)

((acc 'withdraw) 'no-pass 10)


;;
;; 3.1.2  The Benefits of Introducing Assignment
;;

;;
;; explicit state change via rand_update()
;; x_2 = (rand_update x_1)
;; x_3 = (rand_update x_2)
;;

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (rand) (rand)) 1))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed    1)))
          (else (iter (- trials-remaining 1)
                      trials-passed))))
    (iter trials 0))

;;
;; passing the state around explicitly
;;

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trial-passed 1)
                     x2))
              (else
                (iter (- trials-remaining 1)
                      trials-passed
                      x2))))))
  (iter trials 0 initial-x))
