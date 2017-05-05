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


;;
;; exercise 3.17
;;

(define x '(a b c))

(define   y '(a b c))
(set-car! y (cddr y))

(define    z     '(a b c))
(set-car!  z      (cdr  z))
(set-car! (cdr z) (cddr z))

(count-pairs x)

(count-pairs y)

(count-pairs z)

(define (count-pairs-new x)
  ;;
  ;; a simple (and inefficient) set implementation
  ;;
  (define empty-set '())
  (define (adjoin x set)
    (if (member x set)
      set
      (cons x set)))
  (define in member)
  ;;
  ;; main recursion
  ;;
  (define (go x seen-so-far)
    (if (not (pair? x))
      (list 0 seen-so-far)
      (let* ((left (go (car x) seen-so-far))
             (count-left (car  left))
             ( seen-left (cadr left)))
        (let* ((right (go (cdr x) seen-left))
               (count-right (car  right))
               ( seen-right (cadr right)))
        (if (in x seen-right)
          (list (+ 0 count-right count-left)           seen-right)
          (list (+ 1 count-right count-left) (adjoin x seen-right)))))))
  (go x empty-set))

(count-pairs-new 'a)

(count-pairs-new x)

(count-pairs-new y)

(count-pairs-new z)
