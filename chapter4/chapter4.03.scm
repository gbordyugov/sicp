;;
;; 4.3 Variations on a Scheme - Nondeterministic Computing
;;


;;
;; exercise 4.35
;;

(load "amb-interpreter.scm")

(gambeval
  '(begin
     (define (an-integer-between low hi)
       (require (<= low hi))
       (amb low (an-integer-between (+ 1 low) hi)))
     (define (a-pythagorean-triple-between low high)
       (let ((i (an-integer-between low high))
             (j (an-integer-between low high))
             (k (an-integer-between low high)))
         (require (= (+ (* i i) (* j j)) (* k k)))
         (list i j k)))
     (newline)
     (display (a-pythagorean-triple-between 1 10))))


;;
;; exercise 4.36
;;

;;
;; (a)
;;
;; explanation of the non-adequacy: nested an-integer-starting-from's
;; would try to go through all values of k before changing i and j
;;

;;
;; (b)
;;
;; note that the desired (i, j, k) are side lengths of a triangle and
;; hence k <= i + j
;;
;; hence the solution
;;


(load "amb-interpreter.scm")
(define (require p) (if (not p) (amb)))

;;
;; to have more values, I copy-paste the definitions into
;; (driver-loop)
;;
(gambeval
  '(begin
     (define (an-integer-starting-from n)
       (amb n (an-integer-starting-from (+ n 1))))
     (define (an-integer-between low hi)
       (require (<= low hi))
       (amb low (an-integer-between (+ 1 low) hi)))
     (define (a-pythagorean-triple)
       (let ((i (an-integer-starting-from 1)))
         (let ((j (an-integer-between 1 i)))
           (let ((k (an-integer-between 1 (+ i j))))
             (require (= (* k k) (+ (* i i) (* j j))))
             (list i j k)))))
     (newline)
     (display (a-pythagorean-triple))))


;;
;; exercise 4.37
;;

(load "amb-interpreter.scm")

(define (require p) (if (not p) (amb)))
(define (an-integer-between low hi)
  (require (<= low hi))
  (amb low (an-integer-between (+ 1 low) hi)))
(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high))
        (hsq (* high high)))
    (let ((j (an-integer-between i high)))
      (let ((ksq (+ (* i i) (* j j))))
        (require (>= hsq ksq))
        (let ((k (sqrt ksq)))
          (require (integer? k))
          (list i j k))))))
;;
;; several useful pruning techniques:
;;
;; - sum of squares of i and j must be a square of an integer
;; - sum of squares of i and j is smaller than high^2



;;
;; 4.3.2 Examples of Nondeterministic Programs
;;

;; (define (distinct? items)
;;   (cond ((null? items) true)
;;         ((null (cdr items) true))
;;         ((member (car items) (cdr items)) false)
;;         (else (distinct? (cdr items)))))

(define (require p) (if (not p) (amb)))
(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker    5)))
    (require (not (= cooper   1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith  fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

;;
;; Exercise 4.38
;;

;;
;; just remove the string with
;; (require (not (= (abs (- smith  fletcher)) 1)))
;;
(define (require p) (if (not p) (amb)))
(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (require (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker    5)))
    (require (not (= cooper   1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker    baker)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

;;
;; five solutions are found
;;


;;
;; exercise 4.39
;;

;;
;; the order of constrains must not affect the answer, but it may
;; affect the runtime
;;


;;
;; exercise 4.40
;;

;;
;; the first four requirements can be eliminated altogether by
;; restricting possible values for baker, cooper, and fletcher.
;;


(load "amb-interpreter.scm")

(gambeval
  '(begin
     (define (multiple-dwelling)
       (let ((baker    (amb 1 2 3 4  ))
             (cooper   (amb   2 3 4 5))
             (fletcher (amb   2 3 4  ))
             (miller   (amb 1 2 3 4 5))
             (smith    (amb 1 2 3 4 5)))
         (require (distinct? (list baker cooper fletcher miller smith)))
         (require (> miller cooper))
         (require (not (= (abs (- smith  fletcher)) 1)))
         (require (not (= (abs (- fletcher cooper)) 1)))
         (list (list 'baker    baker)
               (list 'cooper   cooper)
               (list 'fletcher fletcher)
               (list 'miller   miller)
               (list 'smith    smith))))
     (newline)
     (display (multiple-dwelling))))


;;
;; further optimization: postponing introducing ambiguity only when
;; it's really neaded
;;

;; (define (multiple-dwelling)
;;   (let ((cooper   (amb   2 3 4 5))
;;         (miller   (amb 1 2 3 4 5)))
;;     (require (> miller cooper))
;;     (let ((fletcher (amb 2 3 4)))
;;       (require (not (= (abs (- fletcher cooper)) 1)))
;;       (let ((smith (amb 1 2 3 4 5)))
;;         (require (not (= (abs (- smith  fletcher)) 1)))
;;         (let ((baker (amb 1 2 3 4 5)))
;;           (requre (distinct? (list baker cooper fletcher miller smith)))
;;           (list (list 'baker    baker)
;;                 (list 'cooper   cooper)
;;                 (list 'fletcher fletcher)
;;                 (list 'miller   miller)
;;                 (list 'smith    smith)))))))
