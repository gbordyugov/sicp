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

