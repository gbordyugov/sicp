;;
;; 4.3 Variations on a Scheme - Nondeterministic Computing
;;


;;
;; exercise 4.35
;;

(define (an-integer-between low hi)
  (require (<= low hi))
  (amb low (an-integer-between (+ 1 low) hi)))
