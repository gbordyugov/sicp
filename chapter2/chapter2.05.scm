;;
;; op hash table
;;

(define make-hash make-equal-hash-table)
(define hash-set! hash-table/put!)
(define hash-get  hash-table/get)

(define *op-table* (make-hash))
(define (put op type proc)
  (hash-set! *op-table* (list op type) proc))
(define (get op type)
  (hash-ref *op-table* (list op type) '()))

;;
;; Systems with Generic Operations
;;

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x)
                              (tag x)))
  'done)

;; note that this one is also polymorphic with dispatch on type
(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))



(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add ('rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub ('rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul ('rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div ('rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))


(define (install-complex-package)
  ;; imiported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
         ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-img (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-img (- (real-part z1) (real-part z2))
                        (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle     z1) (angle     z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle     z1) (angle     z2))))
  ;; interface to rest of system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  ;;
  ;; extension by Alyssa
  ;;
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle     '(complex) angle)
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


;;
;; exercise 2.77
;;
;; see code above in the complex package
;;
;; first we dispatch on 'complex tag, calling angle
;; calling angle dispatches on 'rectangular tag and calls the
;; complex-number package internal angle function
;; i.e. two apply-generic calls


;;
;; exercise 2.78
;;

(define (type-tag x)
  (if (number? x)
    'scheme-number
    (car x)))

(define (contents x)
  (if (number? x)
    x
    (cdr x)))

(define (attache-tag tag contents)
  (if (eq? 'scheme-number tag)
    (if (number? contents)
      number
      (error "trying to attach 'scheme-number tag to a non-number"
             tag contents))
    (cons tag contents)))


;;
;; exercise 2.79
;;

(define (install-equ?)
  (define (eq-sn a b)
    (= a b))
  (define (eq-rat a b)
    (= (* (denom a) (numer b))
       (* (numer a) (denom b))))
  (define (eq-complex a b)
    (and (= (real-part a) (real-part b))
         (= (imag-part a) (imag-part b))))
  (put 'equ? '(scheme-number scheme-number) eq-sn)
  (put 'equ? '(rational      rational)      eq-rat)
  (put 'equ? '(complex       complex)       eq-complex)
  'done)

(define (equ? x y) (apply-generic 'equ? x y))

;;
;; exercise 2.80
;;

(define (install-zero)
  (define (zero-sn a)           (= 0 a))
  (define (zero-rat a)          (= 0 (numer a)))
  (define (zero-complex a) (and (= 0 (real-part a))
                                (= 0 (imag-part a))))
  (put '=zero? '(scheme-number) zero-sn)
  (put '=zero? '(rational     ) zero-rat)
  (put '=zero? '(complex      ) zero-complex)
  'done)

(define (=zero? x) (apply-generic '=zero? x y))

;;
;; Coercion
;;

(define *coercion-table* (make-hash))
(define (put-coercion type1 type2 proc)
  (hash-set! *coercion-table* (list type1 type2) proc))
(define (get-coercion type1 type2)
  (hash-ref *coercion-table* (list type1 type2) '()))

(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

(put-coercion 'scheme-number 'complex scheme-number->complex)


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 ( car type-tags))
                (type2 (cadr type-tags))
                (a1 ( car args))
                (a2 (cadr args)))
            (let ((t1->t2 (get-coercion type1 type2))
                  (t2->t1 (get-coercion type2 type1)))
              (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                    (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                    (else (error "No method for these types"
                                 (list op type-tags))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;;
;; Hierarchies of types
;;


;;
;; exercise 2.81 (a)
;;

;;
;; since the lookup would fail, apply-generic would try to coerce them
;; both (it cannot recognize the exact reason lookup failed)
;; and call apply-generic once again exactly with the same type
;; signature
;; thus blowing up the stack
;;

;;
;; exercise 2.81 (b)
;;

;;
;; Louis seems to be wrong, apply-generic works fine as it is
;;

;;
;; exercise 2.81 (c)
;;

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
          (let ((type1 ( car type-tags))
                (type2 (cadr type-tags))
                (a1 ( car args))
                (a2 (cadr args)))
            (if (equal? type1 type2)
              (error "Trying to coerce two equal types" type1 type2)
              (let ((t1->t2 (get-coercion type1 type2))
                    (t2->t1 (get-coercion type2 type1)))
                (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                      (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                      (else (error "No method for these types"
                                   (list op type-tags)))))))
          (error "No method for these types"
                 (list op type-tags)))))))

;;
;; exercise 2.82
;;

(define *coercion-table* (make-hash))
(define (put-coercion list-of-types proc)
  (hash-set! *coercion-table* list-of-types proc))
(define (get-coercion list-of-types)
  (hash-ref *coercion-table* list-of-types '()))


(define (can-be-coerced? types to-type)
  """ check whether all from-types can be coerced to to-type """
  (define (will-coerce? type)
    (if (equal? type to-type)
      #t
      (get-coercion type to-type)))
  (every will-coerce? types))


(define (find-common-types types)
  """ return list of types such that the all the given ones can be
  coerced to """
  (define (common-type? type) (can-be-coerced? type types))
  (filter common-type? types))


(define (get-op op common-type types)
  """ replaces all types in types by common-types and uses the generated
  type list to get method """
  (define (const x) (lambda (y) x))
  (let ((type-signature (map (const common-types) types)))
    (get op type-signature)))


(define (apply-generic-with-known-common-types op common-types types data)
  (if (null? common-types)
    (error "common-types exhausted")
    (let ((type (car common-types)))
      (let ((func (get-op op type types)))
        (if (null? func)
          (apply-generic-with-known-common-types op (cdr common-types)
                                                 types data)
          (apply func (map (lambda (type-value)
                             (let ((type  (car  type-value))
                                   (value (cadr type-value)))
                             ((get-coercion type common-type) value))
                           (zip (types data))))))))))


(define (apply-generic op . args)
  (let* ((types        (map type-tag args))
         (data         (map contents args))
         (common-types (find-common-type types)))
    (apply-generic-with-known-common-types op common-types types data)))


;;
;; exercise 2.83
;;

(define (install-raise-package)
  (define (integer->rational a)
    (make-rat a 1))
  ;;
  (define (rational->real a)
    (make-real (/ (numer a) (denom a))))
  ;;
  (define (real->complex a)
    (make-from-real-imag  0))
  ;;
  (put 'raise 'integer  integer->rational)
  ;;
  (put 'raise 'rational rational->real)
  ;;
  (put 'raise 'real     real->complex)
  'done)

(define (raise a)
  ((get 'raise (type-tag a)) (contents a)))

;; alternative
(define (raise a)
  (apply-generic 'raise x))
