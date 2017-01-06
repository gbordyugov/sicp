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
(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (make-rat n d) (cons n d))

(define (make-rat n d)
  (let ((g (gcd n d)))
   (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display "rat ")
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))
(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))
(print-rat (mul-rat one-half one-third))
(print-rat (add-rat one-third one-third))

;;
;; exercise 2.1
;;

(define (make-rat n d)
  (let ((g (gcd n d)))
   (if (> (* n d) 0)
     (cons (abs n) (abs d))
     (cons (- (abs n)) (abs d)))))

(print-rat (make-rat  1  2))

(print-rat (make-rat  1 -2))

(print-rat (make-rat -1  2))

(print-rat (make-rat -1 -2))


;;
;; exercise 2.2
;;

(define  make-segment cons)
(define start-segment car)
(define   end-segment cdr)

(define make-point cons)
(define x-point car)
(define y-point cdr)

(define (print-point p)
  (newline)
  (display "point (")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment s)
  (define (avg a b) (/ (+ a b) 2))
  (let* ((p1 (start-segment s))
         (p2 ( end-segment s))
         (x1 (x-point p1))
         (y1 (y-point p1))
         (x2 (x-point p2))
         (y2 (y-point p2)))
    (make-point (avg x1 x2) (avg y1 y2))))

(define p1 (make-point 1.0 2.0))
(define p2 (make-point 2.0 1.0))
(print-point (midpoint-segment (make-segment p1 p2)))



;;
;; exercise 2.3
;;

(define (make-rect p1 p2)
  (cons p1 p2)) ;; to be expanded later
(define rect-ul   car) ;; upper-left corner
(define rect-lr   cdr) ;; lower-r corner

(define (rect-dx r)
  (let* ((ul (rect-ul r))
         (lr (rect-lr r))
         (x1 (x-point ul))
         (x2 (x-point lr)))
    (abs (- (x1 x2)))))

(define (rect-dy r)
  (let* ((ul (rect-ul r))
         (lr (rect-lr r))
         (y1 (y-point ul))
         (y2 (y-point lr)))
    (abs (- (y1 y2)))))

(define (rect-perimeter r)
  (+ (rect-dx r) (rect-dx r)
     (rect-dy r) (rect-dy r)))

(define (rect-area r)
  (* (rect-dx r) (rect-dy r)))

(define r1 (make-rect (make-point 1.0 2.0) (make-point 2.0 1.0)))

(rect-area r1)

(rect-perimeter r1)


;;
;; another implementation
;;

(define (make-rect p1 p2)
  (let* ((x1 (x-point p1))
         (y1 (y-point p1))
         (x2 (x-point p2))
         (y2 (y-point p2))
         (dx (- x2 x1))
         (dy (- y2 y1))
         (d (cons dx dy))
         (p (cons x1 y1)))
    (cons p d)))

(define rect-ul car) ;; upper-left
(define (rect-lr r) ;; lower-r corner
  (let* ((ul (rect-ul r))
         (x0 (x-point ul))
         (y0 (y-point ul))
         (dx (car (cdr r)))
         (dy (cdr (cdr r))))
    (make-point (+ x0 dx) (+ y0 dy))))

(define (rect-dx r) (abs (car (cdr r))))
(define (rect-dy r) (abs (cdr (cdr r))))

(define r (make-rect (make-point 1.0 2.0) (make-point 2.0 1.0)))

(define (rect-perimeter r)
  (+ (rect-dx r) (rect-dx r)
     (rect-dy r) (rect-dy r)))

(define (rect-area r)
  (* (rect-dx r) (rect-dy r)))

(rect-perimeter r)

(rect-area r)

;;
;; a cool representation for cons
;;

(define (mcons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "wrong argument of dispatch of MCONS"))))
  dispatch)
(define (mcar z) (z 0))
(define (mcdr z) (z 1))

(mcar (mcons 1 2))
(mcdr (mcons 1 2))


;;
;; exercise 2.4

(define (gcons x y)
  (lambda (m) (m x y)))
(define (gcar z)
  (z (lambda (p q) p)))
(define (gcdr z)
  (z (lambda (p q) q)))

(gcar (gcons 1 2))
(gcdr (gcons 1 2))


;;
;; exercise 2.5
;;

(define (icons a b)
  (* (expt 2 a) (expt 3 b)))
(define (icar z)
  (define (go z n)
    (if (> (remainder z 2) 0)
      n
      (go (/ z 2) (+ n 1))))
  (go z 0))
(define (icdr z)
  (define (go z n)
    (if (> (remainder z 3) 0)
      n
      (go (/ z 3) (+ n 1))))
  (go z 0))

(icar (icons 5 6))
(icdr (icons 5 6))


;;
;; exercise 2.6
;;

(define c-zero
  (lambda (f)
    (lambda (x) x)))

(define (c-add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define c-one
  (lambda (f)
    (lambda (x)
      (f x))))

(define c-two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (c-plus a b)
  (lambda (f)
    (lambda (x)
      ((a f) ((b f) x)))))


;;
;; interval arithmetics
;;

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x (make-interval (/ 1.0 (upper-bound y))
                                 (/ 1.0 (lower-bound y)))))

;;
;; exercise 2.7
;;

(define (make-interval a b) (cons a b))
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))

;;
;; exercise 2.8
;;

(define (sub-interval x y)
  (mul-interval x (make-interval (- (upper-bound y))
                                 (- (lower-bound y)))))

;;
;; exercise 2.9
;;

;; a simple arithmetics problem


;;
;; exercise 2.10
;;

(define (div-interval x y)
  (let* ((ub (upper-bound y))
         (lb (lower-bound y))
         (span-y (- ub lb)))
    (if (or (= 0 span-y) (= 0 ub) (= 0 lb))
      (error "division by zero in (div-interval x y)")
      (mul-interval x (make-interval (/ 1.0 ub) (/ 1.0 lb))))))
