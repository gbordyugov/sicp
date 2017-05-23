;;
;; exercise 3.38
;;
;;
;; basically, there are 6 permutations of 3 elemenents, I trust I can
;; do the arythmetics
;;

;;
;; exercise 3.39
;;
;; it's three possibilities, depending whether the second procedure
;; runs a) before the first serialized lambda b) between it and
;; assignment to x in the first procedure and c) after both
;;


;;
;; exercise 3.40
;;
;; both serialized funcs commute and the computation would produce the
;; same result independently of the order the serialized funcs are
;; called in.
;;

;;
;; exercise 3.41
;;
;; both deposit and withdraw read balance only once, hence I see no
;; need to protect the balance query
;;

;;
;; exercise 3.42
;;
;;
;; in the original version, we create the protected procedures on the
;; fly in each call of dispatch. Here, we pre-cache them. Seems OK to
;; me
;;


;;
;; exercise 3.43
;;
;;
;; in the sequential version, all possible single exchanges preserve
;; the property of having a combination of $10, $20, and $30 as
;; accounts' balances
;;
;; a = 10, b = 20, c = 30
;; run exchange(a, b) and exchange(b, c) in parallel
;; calculate delta(a, b) = 10
;; complete exchange(b, c) such that b = 30 and c = 20
;; add 10 to a, producing 20, subtract 10 from b, producing 20
;; end up with a = 20, b = 20, c = 30, see that the sum remained
;; preserved
;;
;; the sum remains preserved since both deposit and withdraw
;; operations are atomic (serialized)
;;


;;
;; exercise 3.44
;;
;;
;; seems ok to me. At each point of time, the state is the account
;; plus the sum to deposit, I don't see how this can be screwed up
;;


;;
;; exercise 3.45
;;
;;
;; the problem with serialized-exchange is that it serializes exchange
;; by serializers of both accounts. Exchange, in its turn, calls
;; deposit and withdraw methods, which are already serialized by the
;; corresponding serializers. Function exchange would thus wait until
;; both mutexes are free, which would lead to a lock
;;


;;
;; Implementing serializers
;;

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
               (the-mutex 'acquire))) ;; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

(define (test-and-set! cell) ;; this one must be atomic!
  (if (car cell)
    true
    (begin
      (set-car! cell true)
      false)))


;;
;; exercise 3.46
;;
;; a diagram showing the non-atomicity of test-and-set! follows
;;



;;
;; exercise 3.47
;;
;;
;; problems emerge if test-and-set! is not performed atomically, i.e.
;; there is a possibility of change of cell between reading and
;; updating it

;;
;; a)
;;

(define (make-semaphore n)
  (let ((m (make-mutex))
        (how-many-already 0))
    ;;
    ;;
    ;;
    (define (acquire)
      (m 'acquire)
      (if (< how-many-already (- n 1))
        (begin
          (set! how-many-already (+ how-many-already 1))
          (m 'release))
        (begin
          (m 'release)
          (me 'acquire))))
    ;;
    ;;
    ;;
    (define (release)
      (m 'acquire)
      (if (> how-many-already 0)
        (begin
          (set! how-many-already (- how-many-already 1))
          (m 'release))
        (begin
          (error "trying to release an empty semaphore")
          (m 'release))))
    ;;
    ;;
    ;;
    (define (me m)
      (cond ((eq? 'acquire m) acquire)
            ((eq? 'release m) release)
            (else (error "unknown request in make-semaphore" m))))
    me))


;;
;; (b)
;;

(define (make-semaphore n)
  (let ((cell (list false))
        (how-many-already 0))
    ;;
    ;;
    ;;
    (define (acquire)
      (if (test-and-set! cell)
        (me 'acquire) ;; wait a bit more
        (if (< how-many-already (- n 1)) ;; now we're reading/writing
          (begin
            (set! how-many-already (- how-many-already 1))
            (clear! cell))
          (begin
            (clear! cell)
            (me 'acquire)))))
    ;;
    ;;
    ;;
    (define (release)
      (if (test-and-set! cell)
        (me 'acquire) ;; wait a bit more
        (if (> how-many-already 0)
          (begin
            (set! how-many-already (- how-many-already 1))
            (clear! cell))
          (begin
            (error "trying to release an empty semaphore")
            (clear! cell)))))
    ;;
    ;;
    ;;
    (define (me m)
      (cond ((eq? 'acquire m) acquire)
            ((eq? 'release m) release)
            (else (error "unknown request in make-semaphore" m))))
    me))

;;
;; exercise 3.48
;;

(define (make-account balance)
  (let ((account-id (get-random-account-id))) ;; assuming this one is implemented
    (define (withdraw amount)
      (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount)))
    (let ((protected (make-serializer)))
      (define (dispatch m)
        (cond ((eq m 'withdraw)   (protected withdraw))
              ((eq m 'deposit )   (protected deposit))
              ((eq m 'balance )    balance)
              ((eq m 'serializer)  serializer)
              ((eq m 'account-id)  account-id)
              (else (error "Unknown request: MAKE-ACCOUNT" m))))
      dispatch)))

(define (exchage account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit ) difference)))

(define (serialized-exchange account1 account2)
  (let ((id1         (account1 'id))
        (id2         (account2 'id))
        (serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    (if (< id1 id2)
      ((serializer1 (serializer2 exchange)) account1 account2)
      ((serializer2 (serializer1 exchange)) account1 account2))))

;;
;; exercise 3.49
;;
;;
;; as the hint suggests, we can associated a 'partner' account with
;; each existing one and define an 'exchange-with-partner' function.
;; It would lock the account, look into it to get the partner account
;; and exchange it it with the current one. Since the id of the
;; partner account is not known before locking the given one, we
;; cannot decide in advance, which account to lock first.
;;
