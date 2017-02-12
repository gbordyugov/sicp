;;
;; exercise 1.29
;;

(define (inc x) (+ 1 x))

(define (id x) x)

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) dx))

(define (cube x) (* x x x))

(integral cube 0.0 1.0 0.01)
(integral cube 0.0 1.0 0.001)

(define (simpson f a b n)
  (let* ((l  (- b a))
         (h  (/ l n))
         (h3 (* 3 h)))
    (begin
      (define (sum term a next b)
        (define (go a result)
          (if (> a b)
            result
            (go (next a)
                (+ result (term a)))))
        (go a 0))
      (define (g k)
        (let ((fx (f (+ a (* k h))))
              (weight (cond ((or (= k 0) (= k n)) 1.0)
                            ((even? k) 2.0)
                            (else      4.0))))
          (* weight fx)))
      (* h3 (sum g 0 (lambda (n) (+ n 1)) n)))))

(simpson cube 0.0 1.0 100.0)
(simpson cube 0.0 1.0 1000.0)




;;
;; exercise 1.30
;;

(define (sum-i term a next b)
  (define (go a result)
    (if (> a b)
      result
      (go (next a) (+ result (term a)))))
  (go a 0))

(sum   cube 1 inc 10)

(sum-i cube 1 inc 10)


;;
;; exercise 1.31
;;

;;
;; tail-recursive (aka iterative)
;;
(define (product term a next b)
  (define (go a result)
    (if (> a b)
      result
      (go (next a) (* result (term a)))))
  (go a 1))

;;
;; linear recursion
;;
(define (product term a next b)
  (if (> a b)
    1
    (* (term a) (product term (next a) next b))))

(define (factorial n)
  (product id 1 inc n))

(factorial 5)

(define (approx-pi n)
  (define (numer n)
    (define (f i)
      (if (even? i) (+ 2 i) (+ 1 i)))
    (product f 1.0 inc n))
  (define (denom n)
    (define (f i)
      (if (even? i) (+ i 1) (+ i 2)))
    (product f 1.0 inc n))
  (* 4 (/ (numer n) (denom n))))

(define (approx-pi n)
  (define (f i)
    (if (even? i)
      (/ (+ 2 i) (+ i 1))
      (/ (+ 1 i) (+ 2 i))))
  (* 4 (product f 1.0 inc n)))

(approx-pi 1000)


;;
;; exercise 1.32
;;

;;
;; tail-recursive (aka iterative) version
;;
(define (accumulate combiner null-value term a next b)
  (define (go a result)
    (if (> a b)
      result
      (go (next a) (combiner (term a) result))))
  (go a null-value))

;;
;; linearly recursive version
;;
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))

(define (sum term a next b)
  (accumulate + 0.0 term a inc b))

(define (product term a next b)
  (accumulate * 1.0 term a inc b))

(sum id 1 inc 10)

(product id 1 inc 10)

;;
;; exercise 1.33
;;
(define (filtered-accumulate combiner null-value term a next b pred)
  (define (go a result)
    (if (> a b)
      result
      (go (next a) (if (pred a)
                     (combiner (term a) result)
                     result))))
  (go a null-value))

(define (evens a b)
  (filtered-accumulate cons '() id a inc b even?))

(evens 1 10)

(define (prime? n)
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
  (= n (smallest-divisor n)))

(prime? 1999)

(define (sum-of-prime-squares a b)
  (define (square x) (* x x))
  (filtered-accumulate + 0 square a inc b prime?))

(sum-of-prime-squares 1 10)

(define (sum-rel-primes n)
  (define (relative-prime? x)
    (= (gcd n x) 1))
  (filtered-accumulate * 1 id 1 inc (- n 1) relative-prime?))

(sum-rel-primes 133)


;;
;; exercise 1.34
;;

(define (f g) (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

;; (f f) ;; cannot call 2 on 2




;;
;; midpoint method
;;

(define (search f neg-point pos-point)
  (define (close-enough? x y) (< (abs (- x y)) 0.001))
  (define (average x1 x2) (/ (+ x1 x2) 2))
  (let ((midpoint (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      midpoint
      (let ((test-value (f midpoint)))
        (cond ((positive? test-value)
               (search f neg-point midpoint))
              ((negative? test-value)
               (search f midpoint pos-point))
              (else midpoint))))))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
            (error "Values are not of opposite sign" a b)))))

(half-interval-method sin 2.0 4.0)

(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

;;
;; fixed points of functions
;;

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(fixed-point cos 1.0)

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

(define (ff-sqrt x)
  (fixed-point (lambda (y) (/ x y)) 1.0))

;; (ff-sqrt 625.0) ;; this doesn't converge

(define (average x1 x2) (/ (+ x1 x2) 2))

(average 2.0 3.0)

(define (ff-sqrt x)
  (fixed-point (lambda (y) (average y (/ x y))) 1.0))

(ff-sqrt 625.0) ;; now it works


;;
;; exercise 1.35
;;

(define golden-ratio (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))


;;
;; exercise 1.36
;;

(define (fixed-point f first-guess)
  (define tolerance 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display "guess: ")
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))

(define solution1
  (fixed-point (lambda (x)
                 (/ (log 1000) (log x)))
               3.0))

(define solution2
  (fixed-point (lambda (x)
                 (average x (/ (log 1000) (log x))))
               3.0))


;;
;; exercise 1.37
;;

;;
;; tail recursion
;;

(define (cont-frac n d k)
  (define (go i acc)
    (if (= i 0)
      acc
      (go (- i 1) (/ (n i) (+ (d i) acc)))))
  (go k 0))

;;
;; linear recursion
;;

(define (cont-frac n d k)
  (define (go i)
    (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (go (+ 1 i))))))
  (go 1))

(/ 1.0 (cont-frac (lambda (i) 1.0)
                  (lambda (i) 1.0)
                  10))


;;
;; exercise 1.38
;;

(define (ne i) 1.0)

(define (de i)
  (if (= (remainder i 3) 2)
    (* (+ (integer-floor i 3) 1) 2)
    1))

(define e (+ 2 (cont-frac ne de 10)))



;;
;; exercise 1.39
;;

    
(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (- (* x x))))
  (define (d i)
    (if (= i 1) 0 (- (* 2 i) 3)))
  (define (go i acc)
    (if (= i 0)
      acc
      (go (- i 1) (/ (n i) (+ (d i) acc)))))
  (go k 0))

  ;; recursive version
  ;; (define (go i)
  ;;   (if (= i k)
  ;;     (/ (n i) (d i))
  ;;     (/ (n i) (+ (d i) (go (+ 1 i))))))
  ;; (go 1))


(tan-cf (/ 3.1415926 2.0) 10)

(tan-cf (/ 3.1415926 4.0) 10)



;;
;; Newton solver
;;

(define (deriv g)
  (lambda (x) (/ (- (g (+ x (/ dx 2))) (g (- x (/ dx 2)))) dx)))

(define dx 0.00001)

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(newtons-method (lambda (x) (- (* x x) 25)) 1.0)

(define (n-sqrt x)
  (newtons-method (lambda (y) (- (square y) x )) 1.0))

(n-sqrt 25.0)


;;
;; exericse 1.40
;;

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))


;;
;; exercise 1.41
;;

(define (double g)
  (lambda (x) (g (g x))))

(define (inc x) (+ 1 x))

(((double (double double)) inc) 5)

;;
;; exercise 1.42
;;

(define (compose f g)
  (lambda (x) (f (g x ))))

((compose square inc) 6)

;;
;; exercise 1.43
;;

(define (repeated g n)
  (if (= n 1)
    g
    (compose g (repeated g (- n 1)))))

((repeated square 2) 5)


;;
;; exercise 1.44
;;

(define (smooth f)
  (define dx 1.0e-4)
  (lambda (x) (/ (+ (f x) (f (+ x dx)) (f (- x dx))) 3)))

;; turned out very neat
(define (smooth-n f n)
  (repeated smooth n) f)

((smooth-n sin 4) (/ 3.1415926 2))

((smooth-n cos 2) (/ 3.1415926 2))



;;
;; prelude to exercise  1.45
;;

(define (average-damp f)
  (define (average x1 x2) (/ (+ x1 x2) 2))
  (lambda (x) (average x (f x))))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt1 x)
  (fixed-point-of-transform (lambda (y) (/ x y)) average-damp 1.0))

(define (sqrt2 x)
  (fixed-point-of-transform
    (lambda (y) (- (square y) x)) newton-transform 1.0))

(sqrt1 9.0)
(sqrt2 9.0)

;;
;; to exercise  1.45
;;


(define (croot x)
  (fixed-point-of-transform
    (lambda (y) (/ x (* y y)))
    average-damp 1.0))

(croot 27.0)

(define (froot x)
  (fixed-point-of-transform
    (lambda (y) (/ x (* y y y)))
    (repeated average-damp 2) 1.0))

(froot 81.0)

(define (nroot x n)
  (define (repeated g n)
    (if (= n 1.0)
      g
      (compose g (repeated g (- n 1)))))
  (define (log2 x) (/ (log x) (log 2.0)))
  (fixed-point-of-transform
    (lambda (y) (/ x (expt y (- n 1))))
    (repeated average-damp (ceiling (log2 n))) 1.0))

(nroot (expt 16.666 88) 88)

;;
;; answer: log2 averagings are needed
;;


;;
;; exercise 1.46
;;

(define (iterative-improve good-enough? improve)
  (define (go guess)
    (if (good-enough? guess)
      guess
      (go (improve guess))))
  go)

(define (sqrt3 x)
  (define (average x1 x2) (/ (+ x1 x2) 2))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt3 16.0)

(define (fixed-point-2 f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.00001))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(fixed-point-2 cos 1.0)
(fixed-point-2 (lambda (y) (+ (sin y) (cos y))) 1.0)
