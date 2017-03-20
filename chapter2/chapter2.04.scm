;;
;; Multiple Representation for Abstract Data
;;

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z2) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z2) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle     z1) (angle     z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle     z1) (angle     z2))))

;;
;; Ben's representation
;;

(define (real-part z) (car z))
(define (imag-part z) (cdr z))
(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z) (atan (imag-part z) (real-part z)))
(define (make-from-real-imag x y) (cons x y))
(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))

;;
;; Alyssa's representation
;;

(define (real-part z) (* (magnitude z) (cos (angle z))))
(define (imag-part z) (* (magnitude z) (sin (angle z))))
(define (magnitude z) (car z))
(define (angle     z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

(define (make-from-mag-ang r a) (cons r a ))

;;
;; tagged data
;;

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad tagged datum: CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;;
;; Ben's updated representation
;;

(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z) (cdr z))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle-rectangular z) (atan (imag-part z) (real-part z)))
(define (make-from-real-imag-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

;;
;; Alyssa's updated representation
;;

(define (real-part-polar z)
  (* (magnitude z) (cos (angle z))))
(define (imag-part-polar z)
  (* (magnitude z) (sin (angle z))))
(define (magnitude-polar z)
  (car z))
(define (angle-polar z)
  (cdr z))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a )))

;;
;; polymorphic accessors
;;

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

(define (add-complex z1 z2)
  (make-from-real-imag  (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

;;
;; Ben's package
;;
(define (install-rectangular-package)
  ;; note that within package, we operate on untagged data
  (define (real-part z)
    (car z))
  (define (imag-part z)
    (cdr z))
  (define (make-from-real-imag x y)
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; here the internal representation ends

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'rectangular x))
  ;; why are those lookup keys are lists of single elements?
  ;; because why: those lists will be built by apply-generic below
  ;; which pulls the tags of all arguments of a generic function into
  ;; one list with the number of elements equal to the arity of the
  ;; function
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle     '(rectangular) angle)
  ;; constructors are tagged by mere symbols, since they are not
  ;; looked up by apply-generic, but rather by another piece of code
  ;; we tag data for export though
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang   r a))))
  'done)

;;
;; Alyssa's package
;;
(define (install-polar-package)
  ;; note that within package, we operate on untagged data
  (define (magnitude z)
    (car z))
  (define (angle z)
    (cdr z))
  (define (make-from-mag-ang r a)
    (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; here the internal representation ends

  ;; interface to the rest of the system
  (define (tag x)
    (attach-tag 'polar x))
  ;; why are those lookup keys are lists of single elements?
  ;; because why: those lists will be built by apply-generic below
  ;; which pulls the tags of all arguments of a generic function into
  ;; one list with the number of elements equal to the arity of the
  ;; function
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle     '(polar) angle)
  ;; constructors are tagged by mere symbols, since they are not
  ;; looked up by apply-generic, but rather by another piece of code
  ;; we tag data for export though
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang   r a))))
  'done)

(define (apply-generic op . args)
  ;; what happens here is that type tags of all arguments are pulled
  ;; into one list
  ;; and this list becomes the lookup key for the ops table
  ;; this explains why the code above was installed with keys being
  ;; lists of single elements, since ops were unitary
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle     z) (apply-generic 'angle     z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))


;;
;; exericse 2.73
;;

;;
;; old version
;;
(define (deriv exp var)
  (cond ((number?   exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augned exp) var)))
        ((product? exp)
         (make-sum (make-product (multiplier exp)
                                 (deriv (multiplicand exp) var))
                   (make-product (deriv (multiplier   exp) var)
                                 (multiplicand exp))))
        (else (error "unknown expression type: DERIV" exp))))

;;
;; new version
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp))
               (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;
;; exercise 2.73 (a)
;;
;; we dispatch on the operator type
;;
;; cannot assimilate the predicates number? and variable since they
;; are a single number/symbol and don't have an operator to dispatch
;; on

;;
;; exercise 2.73 (b)
;;

(define (install-deriv-package)
  ;; sum
  (define (make-sum x y) (list '+ x y))
  (define (addend s) (car  s)) ;; the op code is stripped by operands()
  (define (augend s) (cadr s))
  ;; product
  (define (make-product x y) (list '* x y))
  (define (multiplier   s) (car  s)) ;; the op code is stripped by operands()
  (define (multiplicand s) (cadr s))
  ;; derivative calculation
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augned exp) var)))
  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier   exp) var)
                            (multiplicand exp))))
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  'done)


;;
;; exercise 2.73 (c)
;;

(define (install-deriv-package)
  ;; sum
  (define (make-sum x y) (list '+ x y))
  (define (addend s) (car  s)) ;; the op code is stripped by operands()
  (define (augend s) (cadr s))
  ;; product
  (define (make-product x y) (list '* x y))
  (define (multiplier   s) (car  s)) ;; the op code is stripped by operands()
  (define (multiplicand s) (cadr s))
  ;; exponentiation
  (define (make-exponentiation x y) (list '** x y))
  (define (base exp)  ( car exp)) ;; the op code is stripped by operands()
  (define (poser exp) (cadr exp)) ;; the op code is stripped by operands()
  ;; derivative calculation
  (define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
              (deriv (augned exp) var)))
  (define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                            (deriv (multiplicand exp) var))
              (make-product (deriv (multiplier   exp) var)
                            (multiplicand exp))))
  (define (deriv-exponentiation exp var)
    (make-product (make-product (power exp)
                                (make-exponentiation (base exp)
                                                     (- (power exp) 1)))
                  (deriv (base exp) var)))
  (put 'deriv '+  deriv-sum)
  (put 'deriv '*  deriv-product)
  (put 'deriv '** deriv-exponentiation)
  'done)

;;
;; exercise 2.73 (d)
;;

;; here, we dispatch on 'deriv for different operators, such as +, *,
;; **, etc., we'll have to change the order of the first two arguments
;; to the put/get functions

;;
;; wrote an email to Sussman and Bendersky about type tag dispatching
;; which I still don't quite understand
;;

;;
;; exericse 2.74 (a)
;;

;; as a reminder
(define (real-part z) (apply-generic 'real-part z))

(define (get-record employer-id file)
  (let* ((tag (type-tag file))
         (data (contents file))
         (getter (get 'get-record tag)))
    (getter employer-id data)))
;; file = (cons type contents)

;;
;; exericse 2.74 (b)
;;
(define (get-salary employer-id file)
  (let ((record (get-record employer-id file))
        (tag (type-tag record))
        (contentes (contents record))
        (salary-getter (get 'get-salary tag)))
    (salary-getter employer-id contents)))

;;
;; exercise 2.74 (c)
;;

(define (find-employee-record employer-id list-of-files)
  (if (null? list-of-files)
    '()
    (let ((file (car list-of-files))
          (found? (get-record employer-id file)))
      (if found?
        found?
        (find-employee-record employer-id (cdr list-of-files))))))


