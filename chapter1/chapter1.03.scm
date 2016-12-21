;;
;; exercise 1.19
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
  (define (summand k)
    (let ((factor
            (cond ((or (= k 0) (= k n)) 1.0)
                  ((even k) 2.0)
                  (else     4.0)))
          (fk (f (+ a (* k h)))))
      (* factor fk)))
  (let* ((l  (- b a))
         (h  (/ l n))
         (h3 (* 3 h)))
    (* h3 (sum g a in b))))

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
