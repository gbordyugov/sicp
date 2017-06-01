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


;;
;; exercise 3.59
;;

;;
;; (a)
;;

(define (div-streams s1 s2) (stream-map / s1 s2))

(define (integrate-series as)
  (div-streams as integers))

;;
;; (b)
;;

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define sin-series
  (cons-stream 0 (integrate-series cos-series)))
(define cos-series
  (cons-stream 1 (scale-stream (integrate-series sin-series) -1)))


;;
;; exercise 3.60
;;

;;
;; this is a buggy version somehow, need to investigate further
;; I understand why, see the solution below
;;
(define (mul-series s1 s2)
  (let* ((s1car (stream-car s1))
         (s2car (stream-car s2))
         (s1cdr (stream-cdr s1))
         (s2cdr (stream-cdr s2)))
    (cons-stream (* s1car s2car)
                 (add-streams (cons-stream 0 (mul-series s1cdr s2cdr))
                              (add-streams (scale-stream s2cdr s1car)
                                           (scale-stream s1cdr s2car))))))

;;
;; you shallt not use LET with streams!
;;
(define (mul-series s1 s2)
  (let ((s1car (stream-car s1))
        (s2car (stream-car s2)))
    (cons-stream (* s1car s2car)
                 (add-streams (cons-stream 0 (mul-series (stream-cdr s1)
                                                         (stream-cdr s2)))
                              (add-streams (scale-stream (stream-cdr s2) s1car)
                                           (scale-stream (stream-cdr s1) s2car))))))

;;
;; this version seems to be compatible with exercise 3.61
;;
(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1)
                  (stream-car s2))
               (add-streams (scale-stream (stream-cdr s2) (stream-car s1))
                            (mul-series (stream-cdr s1) s2))))
(define p 
  (add-streams
    (mul-series cos-series cos-series)
    (mul-series sin-series sin-series)))

(stream-ref p 0)


;;
;; exercise 3.61
;;

(define (invert-unit-series s)
  (cons-stream 1
               (scale-stream (mul-series (invert-unit-series s)
                                         (stream-cdr s))
                             -1)))

(define invert-cos (invert-unit-series cos-series))
;; (define testomatic (mul-series cos-series (invert-unit-series cos-series)))
(define testomatic (mul-series cos-series invert-cos))

(stream-ref testomatic 0)

;;
;; exercse 3.62
;;

(define (div-series numer denom)
  (let ((d (stream-car denom)))
    (if (= 0 d)
      (error "zero constant term in denominator: DIV-SERIES")
      (scale-stream (mul-series numer
                                (invert-unit-series (scale-stream denom (/ 1 d))))
                    d))))

(define tan-series (div-series sin-series cos-series))

(stream-ref tan-series 0)



;;
;; 3.5.3 Exploiting the Stream Paradigm
;;

(define (sqrt-improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-stream x)
  (define guesses
    (cons-stream
      1.0
      (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)

;; (display-stream (sqrt-stream 2))

(define (pi-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (cons-stream (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

(display-stream (euler-transform pi-stream))


(define (make-tableau transform s)
  (cons-stream s (make-tableau transform (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s))))

(stream-ref (accelerated-sequence euler-transform pi-stream) 0)


;;
;; exercise 3.63
;;
;;
;; in the original version, `guesses` involves caching (memoization)
;;


;;
;; exercise 3.64
;;

(define (stream-limit s tol)
  (let ((s0 (stream-car             s))
        (s1 (stream-car (stream-cdr s))))
    (if (< (abs (- s0 s1)) tol)
      s1
      (stream-limit (stream-cdr s) tol))))

(define (sqrt-l x tol)
  (stream-limit (sqrt-stream x) tol))

(sqrt-l 2.0 0.001)

(sqrt-l 256 0.1)


;;
;; exercise 3.65
;;

(define (ln2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-map - (ln2-summands (+ n 1)))))

(define ln2-stream
  (partial-sums (ln2-summands 1)))

(stream-ref (accelerated-sequence euler-transform ln2-stream) 0)
