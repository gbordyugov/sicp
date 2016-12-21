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
      (define (sum-i term a next b)
        (define (go a result)
          (if (> a b)
            result
            (go (next a)
                (+ result (term a)))))
        (go a 0))
      (define (g k)
        (let ((x (+ a (* k h))))
          (g x)))
      (sum-i g 0 next n))))

(simpson cons 0.0 10.0 10)

;;
;; exercise 1.30
;;

(define (sum-i term a next b)
  (define (go a result)
    (if (> a b)
      result
      (go (next a)
          (+ result (term a)))))
  (go a 0))

(sum   cube 1 inc 10)

(sum-i cube 1 inc 10)
