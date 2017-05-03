;;
;; started Chapter 3.2
;;

;;
;; exercise 3.9
;;

(define (factorial n)
  (if (= 1 n)
    1
    (* n (factorial (- n 1)))))

;;
;; in the recursive version of factorial, we create a chain embedded
;; environments, one of each level of recursion
;;

(define (factorial n)
  (fact-iter 1 1 n))

(define (fact-iter product counter max-count)
  (if (> counter max-count)
    product
    (fact-iter (* counter product)
               (+ counter 1)
               max-count)))
;;
;; this is not very much different, except for an additional binding
;; of fact-iter
;;


;;
;; exercise 3.10
;;

;;
;; the difference is the existence of an intermediate frame with a
;; value of initial-amount
;;


;;
;; exercise 3.11
;;

;;
;; acc and acc2 are keep in the environments that have been created at
;; the moment of call of make-account
;; the code is probably shared
;;
