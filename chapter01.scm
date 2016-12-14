;;
;; Exercise 1.1
;;

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))

(+ a b (* a b))
(= a b)

(if (and (> b a) (< b (* a b)))
  b
  a)

(* (cond
     ((> a 4) a)
     ((< a b) b)
     (else -1))
   (+ a 1))

;;
;; Exercise 1.2
;;

(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (- 6 2) (- 2 7)))


;;
;; Exercise 1.3
;;

(define (square x)
  (* x x))

(define (foo x y z)
  (- (+ (square x) (square y) (square z))
     (square (min x y z))))
  
;;
;; Exercise 1.4
;;
;; Depending on the sign of b, b will be either added or subtracted
;; from a


;;
;; Exercise 1.5
;;

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))

;; normal (lazy) evaluation wouldn't compute (p) since x = 0
;; applicative will


;;
;; Exercise 1.6
;;

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x) x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

(sqrt 9)

(square (sqrt 1000))

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5)

(new-if (= 1 1) 0 5)

(define (sqrt-iter-a guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-a (improve guess x) x)))

(sqrt-iter-a 1.0 100.0)

;; in new-if, both clauses are evaluated no matter the result of the
;; predicate, thus computation will not terminate


;;
;; Exercise 1.7
;;

(sqrt 1.0e-4) ;; this one is not working

(define (sqrt-iter-rel guess x)
  (let ((new-guess (improve guess x)))
    (if (< (abs (/ (- new-guess guess) guess)) 1.0e-3)
      guess
      (sqrt-iter-rel new-guess x))))

(define (sqrt-rel x)
  (sqrt-iter-rel 1.0 x))

(sqrt-rel 1.0e-12) ;; this one is now working


;;
;; Exercise 1.8
;;

(define (croot x)
  (croot-iter 1.0 x))

(define (croot-iter guess x)
  (let ((new-guess (improve-croot guess x)))
    (if (< (abs (/ (- new-guess guess) guess)) 1.0e-6)
      guess
      (croot-iter new-guess x))))

(define (improve-croot guess x)
  (let ((y guess))
    (/ (+ (/ (/ x y) y) y y) 3)))

(croot 8.0)

(croot (* 5.25 5.25 5.25))


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

(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

(expt 2 3)

;;
;; tail recursion: O(n) time, O(1) space
;;

(define (expt b n)
  (define (go b counter product)
    (if (= counter 0)
      product
      (go b (- counter 1) (* b product))))
  (go b n 1))

(expt 2 3)


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
