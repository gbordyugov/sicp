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

(define (make-queue) (cons '() '()))

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


;;
;; exercise 3.22
;;

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! p)
      (set! front-ptr p))
    (define (set-rear-ptr! p)
      (set! rear-ptr p))
    (define (empty-queue?)
      (null? front-ptr))
    (define (front-queue)
      (if (empty-queue?)
        (error "empty queue")
        (car front-ptr)))
    (define (insert-queue item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr!  new-pair)
               front-ptr)
              (else
                (set-cdr! rear-ptr new-pair)
                (set-rear-ptr! new-pair)
                front-ptr))))
    (define (delete-queue)
      (cond ((empty-queue?)
             (error "empty queue"))
            (else (set-front-ptr! (cdr front-ptr))
                  front-ptr)))
    (define (dispatch m)
      (cond ((eq? 'front-ptr      m) front-ptr)
            ((eq? 'rear-ptr       m) rear-ptr)
            ((eq? 'set-front-ptr! m) set-front-ptr!)
            ((eq? 'set-rear-ptr!  m) set-rear-ptr!)
            ((eq? 'empty-queue?   m) (empty-queue?))
            ((eq? 'front-queue    m) (front-queue))
            ((eq? 'insert-queue   m) insert-queue)
            ((eq? 'delete-queue   m) delete-queue)
            (else
              (error "unknown message in make-queue:dispatch" m))))
    dispatch))

(define q (make-queue))

((q 'insert-queue) 'a)
((q 'insert-queue) 'b)
((q 'insert-queue) 'c)
((q 'insert-queue) 'd)
((q 'insert-queue) 'e)

((q 'delete-queue))


;;
;; exercise 3.23
;;

(define (make-triple item prev next) (cons item (cons prev next)))
(define (triple-item t) (car      t))
(define (triple-prev t) (car (cdr t)))
(define (triple-next t) (cdr (cdr t)))
(define (set-triple-item! t item) (set-car! t       item))
(define (set-triple-prev! t prev) (set-car! (cdr t) prev))
(define (set-triple-next! t next) (set-cdr! (cdr t) next))
;;
(define (make-deque)
  (cons '() '()))
;;
(define (front-ptr-deque d) (car d))
(define ( rear-ptr-deque d) (cdr d))
;;
(define (set-front-ptr-deque! d item) (set-car! d item))
(define ( set-rear-ptr-deque! d item) (set-cdr! d item))
;;
(define (empty-deque? d)
  (or 
    (null? (front-ptr-deque d))
    (null? ( rear-ptr-deque d))))
;;
(define (front-deque d)
  (if (empty-deque? d)
    (error "empty deque in front-deque" d)
    (triple-item (front-ptr-deque d))))
;;
(define (rear-deque d)
  (if (empty-deque? d)
    (error "empty deque in rear-deque" d)
    (triple-item (rear-ptr-deque d))))
;;
(define (deque-to-list d)
  (if (empty-deque? d)
    '()
    (cons (car (front-ptr-deque d))
          (deque-to-list (cons (triple-next (front-ptr-deque d))
                               (rear-ptr-deque d))))))
;;
(define (front-insert-deque! d item)
  (if (empty-deque? d)
    (let ((new-triple (make-triple item '() '())))
      (set-front-ptr-deque! d new-triple)
      (set-rear-ptr-deque!  d new-triple))
    (let ((new-triple (make-triple item '() (front-ptr-deque d))))
      (set-triple-prev! (front-ptr-deque d) new-triple)
      (set-front-ptr-deque! d new-triple))))
;;
(define (rear-insert-deque! d item)
  (if (empty-deque? d)
    (let ((new-triple (make-triple item '() '())))
      (set-front-ptr-deque! d new-triple)
      (set-rear-ptr-deque!  d new-triple))
    (let ((new-triple (make-triple item (rear-ptr-deque d) '())))
      (set-triple-next! (rear-ptr-deque d) new-triple)
      (set-rear-ptr-deque! d new-triple))))
;;
(define (front-delete-deque! d)
  (cond ((empty-deque? d)
         (error "empty deque in front-insert-deque!"))
        (else (set-front-ptr-deque! d (triple-next (front-ptr-deque d)))
              (if (not (empty-deque? d))
                (set-triple-prev! (front-ptr-deque d) '())))))
;;
(define (rear-delete-deque! d)
  (cond ((empty-deque? d)
         (error "empty deque in rear-insert-deque!"))
        (else (set-rear-ptr-deque! d (triple-prev (rear-ptr-deque d)))
              (if (not (empty-deque? d))
                (set-triple-next! (rear-ptr-deque d) '())))))


(define d (make-deque))

(front-insert-deque! d 'a)

(front-insert-deque! d 'b)
(front-insert-deque! d 'c)
(front-insert-deque! d 'd)

(deque-to-list d)

(rear-insert-deque! d 1)
(rear-insert-deque! d 2)
(rear-insert-deque! d 3)
(rear-insert-deque! d 4)

(deque-to-list d)

(front-delete-deque! d)

(rear-delete-deque!  d)


;;
;; 3.2.2 Representing Tables
;;

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      #f)))
(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table (cons (cons key value)
                            (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

;;
;; Two-dimensional tables
;;

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          #f))
      #f)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (asso key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable (cons (cons key-2 value)
                                   (cdr subtable)))))
      (set-cdr! table (cons (list key-1
                                  (cons key-2 value))
                            (cdr table)))))
  'ok)

;;
;; local tables
;;

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable (cons (cons key-2 value)
                                       (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
          'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation: TABLE" m))))
    dispatch))
