;;
;; Exercise 1.9
;;

(define (raz+ a b)
  (if (= a 0) b (+ 1 (raz+ (- a 1) b)))) ;; this one is linear-recursive

(define (dva+ a b)
  (if (= a 0) b (dva+ (- a 1) (+ b 1)))) ;; this one is tail-recursive

(raz+ 5 6)

(dva+ 5 6)


;;
;; Exercise 1.10
;;

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))   ;; f(n) = 2*n
(define (g n) (A 1 n))   ;; g(n) = 2^n
(define (h n) (A 2 n))   ;; h(n) = 2*2*2*...*2 (n times)
(define (k n) (* 5 n n)) ;; k(n) = 5*n^2


;;
;; Fibonacci numbers in a tree-recursive way
;;

(define (fib n)
  (if (< n 2)
    n
    (+ (fib (- n 1)) (fib (- n 2)))))

(fib 3)

;;
;; Fibonacci numbers in an iterative way
;;

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))


;;
;; counting change
;;

(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0)
         (= kinds-of-coins 0)) 0)
    (else (+ (cc amount (- kinds-of-coins 1))
             (cc (- amount (first-denomination kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond
    ((= kinds-of-coins 1)  1)
    ((= kinds-of-coins 2)  5)
    ((= kinds-of-coins 3) 10)
    ((= kinds-of-coins 4) 25)
    ((= kinds-of-coins 5) 50)))

(count-change 10)


;;
;; Exercise 1.11
;;

;;
;; recursive version
;;

(define (f n)
  (if (< n 3)
      n
      (+ (* 1 (f (- n 1)))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))

(f 15)

;;
;; iterative (tail-recursive) version
;;

(define (g n)
  (define (go a b c counter)
    (if (= counter 0)
      c
      (go (+ (* 1 a) (* 2 b) (* 3 c))
          a
          b
          (- counter 1))))
  (go 2 1 0 n))

(g 15)


;;
;; exercise 1.12
;;

(define (pascal i j) ;; i and j are 0-based
  (if (= 0 (* i j))
    1
    (+ (pascal (- i 1) j)
       (pascal i (- j 1)))))

(pascal 0 0)
(pascal 0 2)
(pascal 2 0)
(pascal 2 2)


;;
;; exercise 1.13 is a calculus proof, skipped
;;


;;
;; exercise 1.14
;;
;; the drawing of exercise 1.14 is skipped
;; I assume the order of growth (= branching factor) to be equal to 2
;; since on each step we make two recursive calls
;;


;;
;; exercise 1.15
;;

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)


;;
;; (a) not fewer than log_3 (12.15/0.1) ;-)
;;
;; (b) order of growth is linear - just one recursive call at most
;;


;;
;; Exponentiation
;;

;;
;; O(n) time, O(n) space (no tail recursion
;;

(define (my-expt b n)
  (if (= n 0)
    1
    (* b (my-expt b (- n 1)))))

(my-expt 2 3)

;;
;; tail recursion: O(n) time, O(1) space
;;

(define (my-expt b n)
  (define (go b counter product)
    (if (= counter 0)
      product
      (go b (- counter 1) (* b product))))
  (go b n 1))

(my-expt 2 3)


;;
;; O(log(n)) time, O(n) space
;;

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 2 3)

;;
;; O(log(n)) time, O(1) space
;;
;; I had to look up the solution onine, could not figure out it myself
;;

(define (fast-expt b n)
  (define (square x) (* x x))
  (define (go a b n)
    (cond
      ((= n 0) a)
      ((even? n) (go a (square b) (/ n 2)))
      (else (go (* a b) b (- n 1)))))
  (go 1 b n))

(fast-expt 2 3)
(fast-expt 2 4)


;;
;; exercise 1.17
;;
(define (mul a b)
  (define (double x) (+ x x))
  (define (halve  x) (/ x 2))
  (cond
    ((= b 0) 0)
    ((even? b) (double (mul a (halve b))))
    (else (+ a (mul a (- b 1))))))

;;
;; exercise 1.18
;;

(define (mul a b)
  (define (double x) (+ x x))
  (define (halve  x) (/ x 2))
  (define (go a b n)
    (cond
      ((= n 0) a)
      ((even? n) (go a (double b) (halve n)))
      (else (go (+ a b) b (- n 1)))))
  (go 0 a b))

(mul 3 4)
(mul 2 2)


;;
;; exercise 1.19
;;
;; a_1 = b_0 q + a_0 q + a_0 p = a_0 (q + p) + b_0 q
;; b_1 = b_0 p + a_0 q         = a_0 q + b_0 p
;;
;;   | (q + p)  q |   | a_0 |
;; = |            | x |     | =: T
;;   | q        p |   | b_0 |
;;
;;
;;       | (q + p)  q |     | (q + p)  q |
;; T^2 = |            |  x  |            |  =
;;       | q        p |     | q        p |
;;
;;    | (q + p)^2 + q^2    (q + p) q + p q |
;; =  |                                    | =
;;    | q (q + p) + p q       q^2 + p^2    |
;;
;;
;;    | 2q^2 + 2qp + p^2   q^2 + 2pq    |
;; =  |                                 |
;;    |    q^2 + 2pq       q^2 + p^2    |
;;
;; Summarizing:
;;
;; q' = q^2 + 2pq
;; p' = q^2 + p^2

(define (fib n)
  (define (go a b p q count)
    (cond
      ((= count 0) b)
      ((even? count)
       (let ((qp (+ (* q q) (* 2 (* q p))))
             (pp (+ (* q q) (* p p))))
         (go a b pp qp (/ count 2))))
      (else (go (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p
                q
                (- count 1)))))
  (go 1 0 0 1 n))

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)
(fib 6)
(fib 7)


;;
;; exercise 1.20
;;

(define (my-gcd a b)
  (if (= b 0)
    a
    (my-gcd b (remainder a b))))

;;
;; Applicative order:
;;
;; (my-gcd 206 40)
;; (my-gcd 40 (remainder 206 40))
;; (my-gcd 40 6)
;; (my-gcd 6 (remainder 40 6))
;; (my-gcd 6 4)
;; (my-gcd 4 (remainder 6 4))
;; (my-gcd 4 2)
;; (my-gcd 2 (remainder 4 2))
;; (my-gcd 2 0)
;; 2
;; four calls to remainder

;;
;; Normal (lazy) order
;;
;; (my-gcd 206 40)
;; (my-gcd 40 (remainder 206 40))
;; (if (= (remainder 206 40) 0)
;;   40
;;   (my-gcd (remainder 206 40) (remainder 40 (remainder 206 40))))
;; one extra evaluation for (remainder 206 40) for the if condition

(define (smallest-divisor n)
  (define (square x)
    (* x x))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor  1)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

(smallest-divisor 12)

(define (prime? n)
  (= n (smallest-divisor n)))

(prime? 139)


;;
;; Fermat's little theorem:
;; if n is a prime number and then for any a < n
;; a^n mod n = a mod n
;;
;; if n is not prime, then most of a < n would not satisfy
;; a^n mod n = a mod n
;; ergo:
;; check for primality: pick a_i < n i = 1, 2, ..., N and for each
;; a_i check if a_i^n mod n = a_i mod n

(define (expmod base exp m)
  (define (square x) (* x x))
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (remainder (square (expmod base (/ exp 2) m)) m))
    (else (remainder (* base (expmod base (- exp 1) m)) m))))

(expmod 3 3 5)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(fermat-test 33)

(define (fast-prime? n times)
  (cond
    ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
    (else false)))

(fast-prime? 117 10)

;;
;; exercise 1.21
;;

(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;;
;; exercise 1.22
;;

(define (timed-prime-test n)
  ;; (newline)
  ;; (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1999)

(define (search-for-prime min max) ;; min <= n < max
  (if (= min max)
     '()
     (begin
       (timed-prime-test min)
       (search-for-prime (+ min 1) max))))

;; (search-for-prime 1 1000000)

;;
;; exercise 1.23
;;

(define (smallest-divisor n)
  (define (square x) (* x x))
  (define (next devisor) (if (= devisor 2) 3 (+ devisor 2)))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (next test-divisor)))))
  (define (divides? a b)
    (= (remainder b a) 0))
  (find-divisor n 2))

(smallest-divisor 1999)


;;
;; exercise 1.24
;;

(define (timed-prime-test n)
  ;; (newline)
  ;; (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 15)
    (report-prime n (- (runtime) start-time))))

(define (report-prime n elapsed-time)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(timed-prime-test 1999)

(define (search-for-prime min max) ;; min <= n < max
  (if (= min max)
     '()
     (begin
       (timed-prime-test min)
       (search-for-prime (+ min 1) max))))

;; (search-for-prime 2 1000000)

;;
;; exercise 1.25
;;

(define (fast-expt b n)
  (define (square x) (* x x))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod-a base exp m)
  (remainder (fast-expt base exp) m))

(expmod-a 3 3 5) ;; looks ok


;;
;; exercise 1.26
;;

;
;; this expmod function calls (expmod base (/exp 2 m)) twice
;; (not once as the original version
;;


;;
;; exercise 1.27
;;

(define (carmichael? n) ;; double-check it
  (define (go a n)
    (if (= a n)
      #t
      (and (= (expmod a n n) (remainder a n))
           (go (+ a 1) n))))
  (go 2 n))


(smallest-divisor 561)

(carmichael? 561)
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)


;;
;; exercise 1.28
;;

(define (expmod-mr base exp m)
  (define (square x) (* x x))
  (cond
    ((= exp 0) 1)
    ((even? exp)
     (let* ((sq  (square (expmod-mr base (/ exp 2) m)))
            (rem (remainder sq m)))
       (if (and      (= rem 1)
                (not (= sq 1))
                (not (= sq (- m 1)))) 0 rem)))
    (else (remainder (* base (expmod-mr base (- exp 1) m)) m))))

(expmod-mr 3 3 2)

(expmod-mr 30 1997 19999)

(define (miller-rabin-prime? n)
  ....
  )

