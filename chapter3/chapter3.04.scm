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
          (m 'release)
          (error "trying to release an empty semaphore"))))
    (define (me m)
      (cond ((eq? 'acquire m) acquire)
            ((eq? 'release m) release)
            (else (error "unknown request in make-semaphore" m))))
    me))
