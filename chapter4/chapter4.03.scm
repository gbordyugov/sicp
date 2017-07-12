;;
;; 4.3 Variations on a Scheme - Nondeterministic Computing
;;


;;
;; exercise 4.35
;;

(define (an-integer-between low hi)
  (require (<= low hi))
  (amb low (an-integer-between (+ 1 low) hi)))


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


(define (a-pythagorean-triple)
  (let ((i (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 i)))
      (let ((k (an-integer-between 1 (+ i j))))
        (require (= (* k k) (+ (* i i) (* j j))))
        (list i j k)))))


;;
;; exercise 4.37
;;

;;
;; several useful pruning techniques:
;;
;; - sum of squares of i and j must be a square of an integer
;; - sum of squares of i and j is smaller than high^2



;;
;; 4.3.2 Examples of Nondeterministic Programs
;;

(define (distinct? items)
  (cond ((null? items) true)
        ((null (cdr items) true))
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4 5))
        (cooper   (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (requre (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker    5)))
    (require (not (= cooper   1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith  fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'backer   backer)
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
;; if I'd had an interpreter at this point of time, it would have
;; shown that there were 5 solutions ;-)
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

(define (multiple-dwelling)
  (let ((baker    (amb 1 2 3 4  ))
        (cooper   (amb   2 3 4 5))
        (fletcher (amb   2 3 4  ))
        (miller   (amb 1 2 3 4 5))
        (smith    (amb 1 2 3 4 5)))
    (requre (distinct? (list baker cooper fletcher miller smith)))
    (require (> miller cooper))
    (require (not (= (abs (- smith  fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'backer   backer)
          (list 'cooper   cooper)
          (list 'fletcher fletcher)
          (list 'miller   miller)
          (list 'smith    smith))))

;;
;; further optimization: postponing introducing ambiguity only when
;; it's really neaded
;;

(define (multiple-dwelling)
  (let ((cooper   (amb   2 3 4 5))
        (miller   (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
    (let ((smith (amb 1 2 3 4 5)))
      (require (not (= (abs (- smith  fletcher)) 1)))
      (let ((baker (amb 1 2 3 4 5)))
        (requre (distinct? (list baker cooper fletcher miller smith)))
        (list (list 'backer   backer)
              (list 'cooper   cooper)
              (list 'fletcher fletcher)
              (list 'miller   miller)
              (list 'smith    smith)))))))
