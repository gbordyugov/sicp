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
  (put 'add '(rational rational) (lambda (x y) (tag (add-rat  x y))))
  (put 'sub '(rational rational) (lambda (x y) (tag (sub-rat  x y))))
  (put 'mul '(rational rational) (lambda (x y) (tag (mul-rat  x y))))
  (put 'div '(rational rational) (lambda (x y) (tag (div-rat  x y))))
  (put 'make 'rational           (lambda (n d) (tag (make-rat n d))))
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
  (put 'add '(complex complex) (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex) (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex) (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex) (lambda (z1 z2) (tag (div-complex z1 z2))))
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

(define (equ? x y)
  (apply-generic 'equ? x y))

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
  (define (common-type? type)
    (can-be-coerced? type types))
  (filter common-type? types))


(define (apply-generic-with-known-common-types op common-types types args
                                               typed-args)
  (if (null? common-types)
    (error "common types exhausted")
    (let* ((common-type (car common-types))
           (func        (get op (map (lambda (x) common-type) types))))
      (if (null? func)
        (apply-generic-with-known-common-types op (cdr common-types)
                                               types args)
        (let* ((coercer (lambda (ta)
                          ((get-coercion (type-tag ta) common-type) ta)))
               (new-typed-args (map coercer typed-args)))
          (apply func args))))))


(define (apply-generic op . typed-args)
  (let* ((types        (map type-tag typed-args))
         (args         (map contents typed-args))
         (common-types (find-common-type types)))
    (apply-generic-with-known-common-types op common-types types args
                                           typed-args)))


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
  (put 'raise '(integer)  integer->rational)
  ;;
  (put 'raise '(rational) rational->real)
  ;;
  (put 'raise '(real)     real->complex)
  'done)

(define (raise a)
  (apply-generic 'raise a))


;;
;; exercise 2.84
;;

(define (higher? type1 type2)
  """ assume that both types belong to the same type tower
  and one of them is higher than the other one, i.e. the other one
  can be raised to the type of the first one """
  (let ((raised-type2 (raise type2)))
    (and raised-type2
         (or (equal?  type1 raised-type2)
             (higher? type1 raised-type2)))))


(define (types-raisable? types to-type)
  """ check whether all of types can be raised to to-type """
  (define (will-raise? t)
    (higher? to-type t))
  (every will-raise? types))


(define (find-supertypes types)
  """ find a common supertype, might return a list of several, but all
  elements would be the same supertype """
  (define (supertype? t)
    (types-raisable? types t))
  (filter supertype? types))

(define (raise-to to-type x)
  """ raise x to to-type, possibly applying raise several times """
  (if (equal? to-type (type-tag x))
    x
    (raise-to to-type (raise x))))

(define (apply-generic op . typed-args)
  """ apply op to typed-args by finding a common supertype and raising
  typed-args to that common supertype """
  (let* ((types      (map type-tag typed-args))
         (supertypes (find-supertypes type)))
    (if (null? supertypes)
      (error "no supertype found")
      (let* ((supertype         (car supertypes))
             (raised-types      (map (lambda (t) supertype) types))
             (raised-typed-args (map (lambda (x) (raise-to supertype x))
                                     typed-args))
             (raised-args       (map contents raised-typed-args))
             (typed-op          (get op raised-types)))
        (if (null? typed-op)
          (error "no op found")
          (apply typed-op raised-args))))))


;;
;; exercise 2.85
;;

(define (install-drop-package)
  (define (complex->real a)
    (make-real (real-part a)))

  (define (real->rational a)
    (let ((r (rationalize (inexact->exact a) 1/100)))
      (make-rat (numerator r) (denominator r))))

  (define (rational->integer a)
    (make-int (floor (/ (numer a) (denom a)))))

  (put 'project '(complex) complex->real)
  (put 'project '(real)    real->rational)
  (put 'project '(rat)     rational->integer)
  'done)


(define (project x)
  (apply-generic 'project x))


(define (drop x)
  """ drop as deep as possible down the type tower """
  (let ((project-proc (get 'project (type-tag x))))
    (if (null? project-proc)           ;; can we at all project?
      x                                ;; no :~(
      (let ((projected-x (project x))) ;; yes, we can!
        (if (equ? x (raise projected-x))
          (drop projected-x)
          x)))))


(define (apply-generic op . typed-args)
  """ apply op to typed-args by finding a common supertype and raising
  typed-args to that common supertype

  additionally drops the result """
  (let* ((types      (map type-tag typed-args))
         (supertypes (find-supertypes type)))
    (if (null? supertypes)
      (error "no supertype found")
      (let* ((supertype         (car supertypes))
             (raised-types      (map (lambda (t) supertype) types))
             (raised-typed-args (map (lambda (x) (raise-to supertype x))
                                     typed-args))
             (raised-args       (map contents raised-typed-args))
             (typed-op          (get op raised-types)))
        (if (null? typed-op)
          (error "no op found")
          (drop (apply typed-op raised-args)))))))

;;
;; exercise 2.86
;;

(define (install-numbers)
  (define (real-sine x)
    (sin x))
  (define (real-cosine x)
    (cos x))
  (define (rat-sine x)
    (sin (/ (numer x) (denom x))))
  (define (rat-cosine x)
    (cos (/ (numer x) (denom x))))
  ;; add more functions here: tan, atan, hypot, etc
  (put 'sine   '(real) real-sine)
  (put 'cosine '(real) real-cosine)
  (put 'sine   '(rat)  rat-sine)
  (put 'cosine '(rat)  rat-cosine)
  ;; install more functions here: tan, atan, hypot, etc
  'done)

(define (sine   x) (apply-generic 'sine   x))
(define (cosine x) (apply-generic 'cosine x))
;; define more functions here: tan, atan, hypot, etc


;;
;; 2.5.3 Example: Symbolic Algebra
;;

(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
                      (make-polynomial (variable p1)
                                       (add-terms (term-list p1)
                                                  (term-list p2)))
                      (error "Polys not in same var: ADD-POLY"
                             (list p1 p2))))
(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
                      (make-polynomial (variable p1)
                                       (mul-terms (term-list p1)
                                                  (term-list p2)))
                      (error "Polys not in same var: ADD-POLY"
                             (list p1 p2))))

(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  ;; procedures same-variable? and variable from section 2.3.2
  ;; representation of terms and term list
  ;; procedures adjoin-term ... coeff from text below
  (define (add-poly p1 p2) (...))
  (define (mul-poly p1 p2) (...))
  ;; interface to rest of the system
  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

;;
;; assume there are two constructors:
;; the-empty-termlist (makes an empty term list)
;; adjoin-term (adds a term to a term list)
;; ... a predicate empty-termlist?
;; ... and selectors
;; first-term (extract the highest term)
;; rest-terms (returns all by the highest term)
;; constructor of terms
;; (make-term order coef)
;; selectors `order` and `coeff`
;;

;;
;; the idea of the above is to abstract away terms and list of terms
;; behind a clear interface
;;

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1))
                (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term t1 (add-terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term t2 (add-terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term
                      (make-term (order t1)
                                 (add (coeff t1) (coeff t2)))
                      (add-terms (rest-terms L1) (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (add-terms (mul-term-by-all-terms (first-term L1) L2)
               (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
    (the-empty-termlist)
    (let ((t2 (first-term L)))
      (adjoin-term (make-term (+   (order t1) (order t2))
                              (mul (coeff t1) (coeff t2)))
                   (mul-term-by-all-terms t1 (rest-terms L))))))

;;
;; Representing term lists
;;

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    term-list
    (cons term term-list)))

(define (the-empty-termlist)
  '())
(define (empty-termlist? term-list)
  (null? term-list))

(define (first-term term-list)
  (car term-list))
(define (rest-terms term-list)
  (cdr term-list))

(define (make-term order coeff)
  (list order-coeff))
(define (order term)
  (car term))
(define (coeff term)
  (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;
;; exercise 2.87
;;

(put '=zero? '(polynomial)
     (lambda (p)
       (let ((termlist (term-list p)))
         (or (empty-termlist? termlist)
             (and (=zero? (coeff (first-term termlist)))
                  (=zero? (make-polynomial (variable p)
                                           (rest-terms termlist))))))))

;;
;; exercise 2.88
;;

(put 'negate '(polynomial)
     (lambda (p)
       (define (negate-term term)
         (make-term (order term) (negate (coeff term))))
       (let ((termlist (term-list p)))
         (make-poly var (map negate-term) termlist))))

(define (negate x)
  (apply-generic 'negate x))


;;
;; exercise 2.89
;;

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
    (term list)
    (cons (coeff term) term-list)))

(define (the-empty-termlist) '())

(define (first-term term-list)
  (let ((order (- (length term-list) 1)))
    (make-term order (car term-list))))

(define (rest-terms term-list)
  (cdr term-list))

(define (empty-termlist? term-list)
  (null? term-list))

(define (make-term order coeff)
  (list order coeff))

(define (order term)
  (car term))

(define (coeff term)
  (cadr term))


;;
;; exercise 2.90
;;
;; turned out to be a long one, but fun!
;;

;;
;; single term representation is shared by both sparse and dense cases
;;

(define (install-term-type)
  ;;
  ;; internal, i.e., tagless code
  ;;
  (define (make-term order coeff)
    (cons order coeff))
  (define (order term) (car term))
  (define (coeff term) (cdr term))

  ;;
  ;; interface to the rest of the system
  ;;
  (define (tag x)
    (attach-tag 'term x))

  (put 'make 'term
       (lambda (order coeff) (tag (make-term order coeff))))
  (put 'order 'term order)
  (put 'coeff 'term coeff)
  'done)


;;
;; this allows for further potential term representations
;;
(define (make-term order coeff)
  ((get 'make 'term) order coeff))
(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))


;;
;; sparse term list code
;;

(define (install-sparse-term-list-type)
  ;;
  ;; internal, i.e., tagless code
  ;;
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

  (define (the-empty-termlist)
    '())
  (define (empty-termlist? term-list)
    (null? term-list))

  (define (first-term term-list)
    (car term-list))
  (define (rest-terms term-list)
    (cdr term-list))

  ;;
  ;; interface to rest of the system
  ;;
  (define (tag x)
    (attach-tag 'sparse-term-list x))

  (put 'adjoint-term '(term sparse-term-list)
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'sparse-term-list
       (lambda (x) (tag (the-empty-termlist x))))
  (put 'empty-termlist? '(sparse-term-list) empty-termlist?)
  (put 'first-term '(sparse-term-list) first-term)
  (put 'rest-terms '(sparse-term-list) rest-terms)
  'done)


;;
;; this ones need to be done only once
;;
(define (adjoin-term term term-list)
  (apply-generic 'adjoint-term term term-list))
(define (the-empty-termlist)
  (apply-generic 'the-empty-termlist))
(define (empty-termlist? x)
  (apply-generic 'empty-termlist? x))
(define (first-term x)
  (apply-generic 'first-term x))
(define (rest-terms x)
  (apply-generic 'rest-terms x))


;;
;; dense term list code
;;

(define (install-dense-term-list-type)
  ;;
  ;; internal, i.e., tagless code
  ;;
  (define (adjoin-term term term-list)
    (if (=zero? (coeff term))
      (term list)
      (cons (coeff term) term-list)))

  (define (the-empty-termlist) '())
  (define (empty-termlist? term-list)
    (null? term-list))

  (define (first-term term-list)
    (let ((order (- (length term-list) 1)))
      (make-term order (car term-list))))

  (define (rest-terms term-list)
    (cdr term-list))

  ;;
  ;; interface to rest of the system
  ;;
  (define (tag x)
    (attach-tag 'dense-term-list x))
  (put 'adjoint-term '(term dense-term-list)
       (lambda (term term-list)
         (tag (adjoin-term term term-list))))
  (put 'the-empty-termlist 'dense-term-list
       (lambda (x) (tag (the-empty-termlist x))))
  (put 'empty-termlist? '(dense-term-list) empty-termlist?)
  (put 'first-term '(dense-term-list) first-term)
  (put 'rest-terms '(dense-term-list) rest-terms)

  'done)

;;
;; see lines 751-760 for interfacing that to rest of the system
;;


;;
;; code for polynomials with sparse/dense term lists
;;
;; the sparse vs dense decision is simply delegated to the level of
;; the term list representation. 
;;

(define (install-polynomial-package)
  ;;
  ;; typeless. i.e., tagless code
  ;;
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p)
    (car p))
  (define (term-list p)
    (cdr p))
  ;;
  ;; those two will dispatch on the underlying type of term list
  ;;
  (define (add-poly p1 p2) (...))
  (define (mul-poly p1 p2) (...))
  (define (same-variable? p1 p2) (...))
  ;;
  ;; interface to rest of the system
  ;;
  (define (tag p)
    (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'variable ' polynomial variable)
  (put 'term-list 'polynomial term-list)
  (put 'same-variable? 'polynomial same-variable?)
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define (variable       p)     (apply-generic 'variable       p))
(define (term-list      p)     (apply-generic 'term-list      p))
(define (same-variable? p1 p2) (apply-generic 'same-variable? p1 p2))

;;
;; exercise 2.91
;;

(define (div-terms L1 L2)
  (if (empty-termlist? L1)
    (list (the-empty-termlist) (the-empty-termlist))
    (let ((t1 (first-term L1))
          (t2 (first-term L2)))
      (if (> (order t2) (order t1))
        (list (the-empty-termlist) L1)
        (let* ((new-coeff         (div (coeff t1) (coeff t2)))
               (new-order         (-   (order t1) (order t2)))
               (new-term          (make-term new-o new-c))
               (new-term-times-L2 (mul-term-by-all-terms new-term L2))
               ;; sub-terms needs to be defined (not difficult)
               (L1-minus-product  (sub-terms L1 new-term-times-L2))
               (rest-of-result    (div-terms L1-minus-product L2))
               (quot              (car  rest-of-result))
               (remi              (cdr rest-of-result)))
          (cons (adjoin-term new-term quot) remi))))))

(define (div-poly p1 p2)
  (if (not (same-variable? p1 p2))
    (error "polynomial variable mismatch in div-poly" (list p1 p2))
    (let ((var (variable p1))
          (L1  (term-list p1))
          (L2  (term-list p2))
          (dt  (div-terms L1 L2))
          (q   (car dt))
          (r   (cdr dt)))
      (cons
        (make-polynomial var q)
        (make-polynomial var r)))))


;;
;; exercise 2.92
;;

;;
;; skipped
;;


;;
;; Extented exercise: Rational functions
;;

;;
;; exercise 2.93
;;

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
      (cons n g))
  (define (equ-rat? x y)
    (and (equ? (numer x) (numer y))
         (equ? (denom x) (denom y))))
  (define (=zero-rat? x)
    (=zero? (numer x)))
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
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat  x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat  x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat  x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat  x y))))
  (put 'equ?   'rational equ-rat?)
  (put '=zero? 'rational =zero-rat?)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)


;;
;; exercise 2.94
;;

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (remainder-terms a b))))

(define (reminder-terms a b)
  (cdr (div-terms a b)))

(define (gcd-poly a b)
  (if (not (same-variable? (variable a) (variable b)))
    (error "not the same variable in gcd-poly")
    (let* ((t1 (term-list a))
           (t2 (term-list b)))
      (make-polynomial (variable a) (gcd-terms t1 t2)))))

(put 'greatest-common-divisor '(polynomial polynomial) gcd-poly)
(put 'greatest-common-divisor '(scheme-number scheme-number) gcd)

;;
;; exercise 2.95
;;

;;
;; skipped
;;


;;
;; exercise 2.96
;;

;;
;; a)
;;

(define (pseudoreminder-terms p q)
  (let* ((ftp      (first-term p))
         (cp       (coeff ftp))
         (op       (order ftp))
         (ftq      (first-term q))
         (cq       (coeff ftq))
         (oq       (order ftq))
         (factor   (expt cq (+ 1 (- (cp cq)))))
         (scaled-p (mul-term-by-all-terms (make-term 0 factor) p)))
    (cdr (div-terms scaled-p q))))


(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (gcd-terms b (pseudoremainder-terms a b))))


;;
;; b)
;;

(define (gcd-terms a b)
  (if (empty-termlist? b)
    a
    (let* ((pre-answer (gcd-terms b (pseudoremainder-terms a b)))
           (coeffs (map coeff pre-answer))
           (the-gcd (apply gcd coeffs)))
      (map (lambda (t)
             (make-term (order t)
                        (/ (coeff t) the-gcd)))
           pre-answer))))


;;
;; exercise 2.97
;;

;;
;; a)
;;
(define (reduce-terms n d)
  (define (factor-out-gcd terms)
    (let ((the-gcd (apply gcd (map coeff terms))))
      (map (lambda (t)
             (make-term (order t)
                        (/ coeff t) the-gcd))
           terms)))
  (let ((gcd    (gcd-terms n d))
        (factor (expt (coeff (first-term gcd))
                      (+ 1 ( - (max (order (first-term n))
                                    (order (first-term d)))
                               (order (first-term gcd))))))
        (t (make-term 0 factor))
        (n1 (mul-term-by-all-terms t n))
        (d1 (mul-term-by-all-terms t d)))
    (list (factor-out-gcd n1)
          (factor-out-gcd d1))))

(define (reduce-poly a b)
  (if (not (same-variable? (variable a) (variable b)))
    (error "not the same variable in gcd-poly")
    (let* ((t1  (term-list a))
           (t2  (term-list b))
           (ans (reduce-terms t1 t2)))
      (list 
        (make-polynomial (variable a) (car  ans))
        (make-polynomial (variable a) (cadr ans))))))


;;
;; b)
;;

(define (reduce-integers n d)
  (let ((g (gcd n d)))
    (list (/ n g) (/ d g))))

(put 'reduce '(scheme-number scheme-nuber) reduce-integers)

(put 'reduce '(polynomial polynomial)
     (lambda (n d)
       (let ((answer (reduce-poly n d)))
         (list
           (attach-tag 'polynomial (car  answer))
           (attach-tag 'polynomial (cadr answer))))))

;;
;; was a long chapter!
;; my solutions are most certainly buggy and not complete, but I aimed
;; at a quick-and-dirty solutions. They are not tested either.
;;
