(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;
;; exercise 2.53
;;

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

;;
;; exercise 2.54
;;

(define (gequal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b))
              (gequal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))

(gequal? 'a 'a)

(gequal? 'a 'b)

(gequal? 'a '(a b))

(gequal? '(a b) '(a b))

;;
;; exercise 2.55
;;

(car ''abracadabra)
;; = (car '(quote abracadabra)))
;; = quote


;;
;; Symbolic differentiation
;;

;;
;; considered as provided
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum     a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) ( cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier   s) ( cadr s))
(define (multiplicand s) (caddr s))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;
;; exercise 2.56
;;

(define (base     exp) ( cadr exp))
(define (exponent exp) (caddr exp))

;; (define (make-exponentiation base exp) (list '** base exp))

(define (exponentiation? e)
  (eq? (car e) '**))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp))
         (expt base exp))
        (else (list '** base exp))))

;;
;; exercise 2.57
;;

;;
;; old version
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
;;
;; new version
;;

;;
;; a small helper funciton
;;
(define (collect-numbers-and-symbols l)
  (list (filter number? l) (filter symbol? l)))

(collect-numbers-and-symbols '(x 1 y 2 z 3))

(define (make-sum . summands)
  (define (make-sum-symbols l)
    (cond ((null? l) '())
          ((= 1 (length l)) (car l))
          (else (list '+ (car l) (make-sum-symbols (cdr l))))))
  (let* ((ns (collect-numbers-and-symbols summands))
         (number  (apply + (car  ns)))
         (symbols (cadr ns)))
    (cond ((null? symbols) number)
          ((= 0 number) (make-sum-symbols symbols))
          (else (list '+ number (make-sum-symbols symbols))))))

;; todo
(define (addend s) (cadr s))
;; todo
(define (augend s) (cddr s))

(make-sum '())

(make-sum 0)

(make-sum 0 1 2 3)

(make-sum 1 'x)

(make-sum 0 'x)

(make-sum 0 1 'z 2)

(make-sum 0 1 'x 3 'y 5 6)

(make-sum 0 1 'x 3 'y 5 'z 6)


(define (make-product . factors)
  (define (make-prod-symbols l)
    (cond ((null? l) '())
          ((= 1 (length l)) (car l))
          (else (list '* (car l) (make-prod-symbols (cdr l))))))
  (let* ((ns (collect-numbers-and-symbols factors))
         (number  (apply * (car  ns)))
         (symbols (cadr ns)))
    (cond ((null? symbols) number)
          ((= 0 number) 0)
          ((= 1 number) (make-prod-symbols symbols))
          (else (list '* number (make-prod-symbols symbols))))))

;; todo
(define (multiplier   s) (cadr s))

;; todo
(define (multiplicand s) (cddr s))

(make-product '())

(make-product 1)

(make-product 1 2 3)

(make-product 0 1 2 3)

(make-product 1 2 0 3)

(make-product 0 'x)

(make-product 1 'x)

(make-product 2 'x)

(make-product 1 1 'z 2)

(make-product 1 1 'x 3 'y 5 6)

(make-product 1 1 'x 3 'y 5 'z 6)

(make-product 1 1 'x 3 0 'y 5 'z 6)
