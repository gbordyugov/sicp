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
;; collect-numbers-and-symbols :: [NumberOrExpression]
;;                             -> ([Number], [Expression])
(define (collect-numbers-and-exprs l op)
  (list (apply op (filter number? l))
        (filter (lambda (x) (not (number? x))) l)))

(collect-numbers-and-exprs '(x 1 y 2 z 3) +)

(collect-numbers-and-exprs '(x 1 (* x 3) 2 z 3) +)

;; make-sum :: [NumberOrExpression] -> Expression
;; the output can be an Expression, not always a sum
(define (make-sum . summands)
  (let* ((ns (collect-numbers-and-exprs summands +))
         (number  (car  ns))
         (exprs (cadr ns)))
    (cond ((null? exprs) number)
          ((and (= 0 number) (null? (cdr exprs))) (car exprs))
          ((= 0 number) (cons '+ exprs))
          (else (cons '+ (cons number exprs))))))

(make-sum '())

(make-sum 0)

(make-sum 0 1 2 3)

(make-sum 'x 'y 'z)

(make-sum 'x 'y 'z '(+ a b))

(make-sum '(+ a b))

(make-sum 1 'x)

(make-sum 0 'x)

(make-sum 0 1 'z 2)

(make-sum 0 1 'x 3 'y 5 6)

(make-sum 0 1 'x 3 'y 5 'z 6)

;; addend :: Sum -> Expression
(define (addend s) ( cadr s))

(make-sum 'x 2 'y 3)

(addend (make-sum 'x 2 3))

(addend (make-sum 'x 2 'y 3))

;; augend :: Sum -> Expression
(define (augend s)
  (let ((ttail (cddr s)))
    (if (null? (cdr ttail))
      (car ttail)
      (apply make-sum ttail))))

(augend (make-sum 'x 2 3))

(augend (make-sum 'x 2 'y 3))

(augend (make-sum 'x 2 3))

(augend (make-sum 'x 2 '(* a b)))


;; make-sum :: [NumberOrExpression] -> Expression
;; the output can be an Expression, not always a sum
(define (make-product . factors)
  (let* ((ns (collect-numbers-and-exprs factors *))
         (number  (car  ns))
         (exprs (cadr ns)))
    (cond ((= 0 number) 0)
          ((null? exprs) number)
          ((and (= 1 number) (null? (cdr exprs))) (car exprs))
          ((= 1 number) (cons '* exprs))
          (else (cons '* (cons number exprs))))))

(make-product '())

(make-product 1)

(make-product 1 2 3)

(make-product 0 1 2 3)

(make-product 1 2 0 3)

(make-product 0 'x)

(make-product 0 'x '(+ a b))

(make-product 1 'x '(+ a b))

(make-product 1 'x)

(make-product 2 'x)

(make-product 1 1 'z 2)

(make-product 1 1 'x 3 'y 5 6)

(make-product 1 1 'x 3 'y 5 'z 6)

(make-product 1 1 'x 3 0 'y 5 'z 6)

;; multiplier :: Sum -> Expression
(define (multiplier s)
  (cadr s))

(make-product 'x 2 'y 3)

(multiplier (make-product 'x 2 3))

(multiplier (make-product 'x 2 'y 3))

(multiplier (make-product 'x 'y))

(multiplier (make-product 'x 'y 'z))

;; augend :: Sum -> Expression
(define (multiplicand s)
  (let ((ttail (cddr s)))
    (if (null? (cdr ttail))
      (car ttail)
      (apply make-product ttail))))

(multiplicand (make-sum 'x 2 3))

(multiplicand (make-product 'x 2 'y 3 '(* a b)))

(multiplicand (make-product 'x 2 3))

(multiplicand (make-product 'x 'y 'z '(- 3 4)))


;;
;; exercise 2.58 (a)
;;
;;
;; sum
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s) (  car s))
(define (augend s) (caddr s))
(define (sum? x) (and (pair? x)
                      (pair? (cdr x))
                      (pair? (cddr x))
                      (eq? (cadr x) '+)))

;;
;; product
;;
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m1))))
(define (multiplier s)   (  car s))
(define (multiplicand s) (caddr s))
(define (product? x) (and (pair? x)
                          (pair? (cdr x))
                          (pair? (cddr x))
                          (eq? (cadr x) '*)))

;;
;; exercise 2.58 (b)
;;
;; seems to be a bit tougher than the previous one
;;
;; skipped for time being
;;

;;
;; Example: Representing Sets
;;

;;
;; Sets as unordered lists
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;
;; exercise 2.59
;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((set3 (union-set (cdr set1) set2)))
                (if (element-of-set? (car set1) set3)
                  set3
                  (cons (car set1) set3))))))

(union-set '(4 5 1 2 3) '(1 4 5 2 6))

;;
;; exercise 2.60
;;
;; sets as lists with duplicates
;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(1 2 3))

(element-of-set? 'a '(1 2 3))

;;
;; that one is fast
;;
(define adjoin-set cons)


;; that one is fast too
;;
(define union-set append)

;; this is just a repetition
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) (set2)))))

;;
;; sets as ordered lists
;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
             (intersection-set (cdr set1) set2))
            ((< x2 x1)
             (intersection-set set1 (cdr (set2))))))))

;;
;; exercise 2.61
;;

(define (adjoin-set x set)
  (if (or (null? set) (< x (car set)))
    (cons x set)
    (cons (car set) (adjoin-set x (cdr set)))))

(adjoin-set 3 '(1 2 4 5))

(adjoin-set 3 '(1 2 3 4 5))

(adjoin-set 0 '(1 2 3 4 5))

(adjoin-set 6 '(1 2 3 4 5))

