;;
;; With ordinary lists, both the `car` and the `cdr` are evaluated at
;; construction time. With streams, the `cdr` is evaluated at
;; selection time.
;;


;;
;; the two stream axioms:
;;
;; (stream-car (cons-stream x y)) = x
;; (stream-cdr (cons-stream x y)) = y
;;
;;
;; (cons-stream <a> <b>)
;; is equivalent to
;; (cons <a> (delay <b>))
;;
;; (define (stream-car s) (car s))
;; (define (stream-cdr s) (force (cdr stream)))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin
      (proc (stream-car s))
      (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x)
  (newline)
  (display x))

(define (memo-proc proc)
  (let ((already-run? false)
        (result       false))
    (lambda ()
      (if (not already-run?)
        (begin
          (set! result (proc))
          (set! already-run true)
          result)
        result))))

;; so (delay <exp>) is equiavalent to
;; (memo-proc (labda () <exp>))

;;
;; exercise 3.50
;;

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
      (apply proc       (map stream-car argstreams))
      (apply stream-map (cons proc (map stream-cdr argstreams))))))


;;
;; exercise 3.51
;;

;;
;; will show numbers up to 5 first, then those between 5 and 7
;;


;;
;; exercise 3.52
;;
;; the point here is that sum is not accumulated fully over the whole
;; stream, but rather over the portion of it that has been used
;; (either by stream-filter or something else)
;;
;; memoizing would prevent adding things to sum twice
;;

;;
;; 3.5.2 Infinite streams
;;

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

(stream-ref no-sevens 100)

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))
(define fibs (fibgen 0 1))

(define (sieve stream)
  (let ((n (stream-car stream)))
    (cons-stream
      n
      (sieve (stream-filter
               (lambda (x)
                 (not (divisible? x n)))
               (stream-cdr stream))))))
(define primes (sieve (integers-starting-from 2)))

;;
;; Definining streams implicitly
;;

(define ones (cons-stream 1 ones))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define fibs
    (cons-stream 0 (cons-stream 1 (add-streams (stream-cdr fibs) fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
    2
    (stream-filter prime? (integers-starting-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


;;
;; exercise 3.53
;;

(define s (cons-stream 1 (add-streams s s)))

;;
;; x_i = 2^i
;;


;;
;; exercise 3.54
;;

(define (mul-streams s1 s2) (stream-map * s1 s2))

(define factorials
  (cons-stream 1 (mul-streams factorials integers)))

;;
;; exercise 3.55
;;

(define (partial-sums stream)
  (cons-stream (stream-car stream)
        (add-streams (partial-sums stream) (stream-cdr stream))))

;;
;; exercise 3.56
;;

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream
                      s1car
                      (merge (stream-cdr s1) (stream-cdr s2)))))))))

(define S
  (cons-stream 1 (merge (scale-stream S 2)
                        (merge (scale-stream S 3)
                               (scale-stream S 5)))))

;;
;; exercise 3.57
;;
;; this is a reference to an earlier discussion of Fibonacci sequence
;; generating an exponential tree
;;


;;
;; exercise 3.58
;;

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

;;
;; this function expands num/den in radix-based system
;;
