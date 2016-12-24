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

(f f) ;; cannot call 2 on 2
;;
