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
