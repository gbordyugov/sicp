;;
;; started Chapter 3.3
;;

;;
;; exercise 3.12
;;

;; (cdr x) => (b)
;; (cdr x) => (b c d)



;;
;; exercise 3.13
;;

;;
;; a circular list list, (last-pair z) would produce an infinite loop
;;



;;
;; exercise 3.14
;;

;;
;; reverses list in-place
;;



;;
;; exercise 3.15
;;

;;
;; drawing in my notebook
;;



;;
;; exercise 3.16
;;

(define (count-pairs x)
  (if (not (pair? x))
    0
    (+ (count-pairs (car x))
       (count-pairs (cdr x))
       1)))

;;
;; 3 pairs is easy : just '(a b c)
;;
;; 4 pairs         : list of three with car pointing to the last pair,
;;                   i.e. (define x '(a b c))
;;                        (set-car! x (cddr x))
;; 7 pairs         :  (define x '(a b c))
;;                    (set-car! x       (cdr x))
;;                    (set-car! (cdr x) (cddr x))

;;
;; never return at all: circular list
;;
