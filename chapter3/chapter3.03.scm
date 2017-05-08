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


;;
;; exercise 3.18
;;

(define (cycle? x)
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
      #f
      (if (in x seen-so-far)
        #t
        (go (cdr x) (adjoin x seen-so-far)))))
  (go x '()))

(cycle? '(a b c))

(define x '(a b))
(set-cdr! (last-pair x) x)

(cycle? x)

(define y '(a b c))
(set-cdr! (last-pair y) y)

(cycle? y)


;;
;; exercise 3.19
;;

;;
;; iterate with two pointers through the list, shifting the first one
;; by one and the second one by two. In a cycle list, they will
;; coincide at some point
;;
(define (cycle? x)
  (define (iter x1 x2)
    (cond ((or (not (pair? x1))
               (not (pair? x2))) #f)
          ((not (pair? (cdr x2))) #f)
          ((eq? x1 x2) #t)
          (else (iter (cdr x1) (cdr (cdr x2))))))
  (if (not (pair? x))
    #f
    (iter x (cdr x))))

(cycle? '(a b c))

(define x '(a b))
(set-cdr! (last-pair x) x)

(cycle? x)

(define y '(a b c))
(set-cdr! (last-pair y) y)

(cycle? y)


;;
;; exercise 3.20
;;

;;
;; drawings in my notebook
;;


;;
;; 3.3.2 Representing Queues
;;

(define (front-ptr q) (car q))
(define (rear-ptr  q) (cdr q))
(define (set-front-ptr! q iterm)
  (set-car! q item))
(define (set-rear-ptr! q iterm)
  (set-cdr! q item))

(define (empty-queue? q)
  (null? (front-ptr q)))

(define (make-queue (cons '() '())))

(define (front-queue q)
  (if (empty-queue? q)
    (error "empty queue")
    (car (front-ptr q))))

(define (insert-queue! q item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? q)
           (set-front-ptr! q new-pair)
           (set-rear-ptr!  q new-pair)
           queue)
          (else
            (set-cdr! (rear-ptr q) new-pair)
            (set-rear-ptr! q new-pair)
            q))))

(define (delete-queue! q)
  (cond ((empty-queue? q)
         (error "empty queue"))
        (else (set-front-ptr! q (cdr (front-ptr q)))
              q)))



;;
;; exercise 3.21
;;

;;
;; queue is a pair, car of which pointing to the beginning of the queue
;; and cdr pointing to the last element. Thus, when printing that
;; pair, the queue itself plus its lasat element is printed
;;

(define (print-queue q)
  (display (car q))
  (newline))
