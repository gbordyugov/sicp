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

(define table (make-table))

(insert! 'a 1 table)

(lookup 'a  table)

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

;; (define t (make-table))
;; 
;; ((t 'insert-proc) 'a 'b 3)
;; ((t 'insert-proc) 'a 'c 4)
;; ((t 'lookup-proc) 'a 'b)
;; ((t 'lookup-proc) 'a 'c)
;; ((t 'lookup-proc) 'd 'c)


;;
;; exercise 3.24
;;

(define (make-table same-key?)
  (define (assoc key records)
    (cond ((null? records) false)
          ((same-key? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
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

;; (define t (make-table))


;;
;; exercise 3.25
;;

;;
;; one quick solution would be to use admit list of keys as keys
;; themselves
;;
;; below, I'm implementing a more elaborate solution
;;
;;

(define (make-table)
  (define (empty-table key) (list key))
  (define (assoc key records)
    (cond ((null? records) false)
          ((equal? key (caar records)) (car records))
          (else (assoc key (cdr records)))))
  (let ((local-table (empty-table '*table*)))
    ;;
    ;; lookup
    ;;
    (define (lookup list-of-keys)
      (define (go keys table)
        (if (null? keys)
          (cdr table)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
              (go (cdr keys) subtable)
              #f))))
      (go list-of-keys local-table))
    ;;
    ;; insert!
    ;;
    (define (insert! list-of-keys value)
      (define (go keys table)
        (if (null? keys)
          (set-cdr! table value)
          (let ((subtable (assoc (car keys) (cdr table))))
            (if subtable
              (go (cdr keys) subtable)
              (let ((new-subtable (empty-table (car keys))))
                (go (cdr keys) new-subtable)
                (set-cdr! table (cons new-subtable (cdr table))))))))
      (go list-of-keys local-table)
      'ok)
    ;;
    ;; dispatch
    ;;
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'table) local-table)     ;; show the internal table
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;; (define t (make-table))
;; (t 'table)
;; ((t 'insert-proc) '(b b d) 6)
;; ((t 'insert-proc) '(a b) 3)
;; ((t 'insert-proc) '(a b) 3)
;; ((t 'insert-proc) '(a c) 4)
;; ((t 'lookup-proc) '(a b))
;; ((t 'lookup-proc) '(a c))
;; ((t 'lookup-proc) '(d c))
;; ((t 'lookup-proc) '(b b d))


;;
;; exercise 3.26
;;

(define (make-table)
  ;;
  ;; tree data structure
  ;;
  (define (make-tree key value left right)
    (list key value left right))
  (define (make-leaf key value)
    (make-tree key value '() '()))
  (define (empty-tree? t) (null?  t))
  (define (tree-key    t) (car    t))
  (define (tree-value  t) (cadr   t))
  (define (tree-left   t) (caddr  t))
  (define (tree-right  t) (cadddr t))
  ;;
  ;; setters
  ;;
  (define (tree-set-value! t item) (set-car! (cdr   t) item))
  (define (tree-set-left!  t item) (set-car! (cddr  t) item))
  (define (tree-set-right! t item) (set-car! (cdddr t) item))
  ;;
  ;;
  (let ((local-table (cons '*local-tree* '())))
    ;;
    ;; lookup
    ;;
    (define (lookup key)
      (define (go key tree)
        (let ((tkey (tree-key tree)))
          (cond ((empty-tree? tree) #f)
                ((= key tkey) (tree-value tree))
                ((<   key tkey) (go key (tree-left  tree)))
                (else           (go key (tree-right tree))))))
      (go key (cdr local-table)))
    ;;
    ;; insert!
    ;;
    (define (insert! key value)
      (define (go key value tree)
        (let ((tkey (tree-key tree)))
          (cond ((= key tkey) (tree-set-value! tree value))
                ((< key tkey)
                 (if (null? (tree-left tree))
                   (tree-set-left! tree (make-leaf key value))
                   (go key value (tree-left tree))))
                (else ;; (> key tkey)
                 (if (null? (tree-right tree))
                   (tree-set-right! tree (make-leaf key value))
                   (go key value (tree-right tree)))))))
      (if (empty-tree? (cdr local-table))
        (set-cdr! local-table (make-leaf key value))
        (go key value (cdr local-table))))
    ;;
    ;; dispatch
    ;;
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            ((eq? m 'table) local-table)     ;; show the internal table
            (else (error "Unknown operation: TABLE" m))))
    dispatch))

;; (define t (make-table))
;; (t 'table)
;; ((t 'insert-proc) 5 'a)
;; ((t 'insert-proc) 6 'b)
;; ((t 'insert-proc) 4 'c)
;; ((t 'insert-proc) -3 'd)
;; ((t 'insert-proc) 15 'f)
;; (t 'table)
;; 
;; ((t 'lookup-proc) 15)


;;
;; exercise 3.27
;;

;;
;; I've heard of memoization before
;;


;;
;; 3.3.4 A Simulator for Digital Circuits
;;

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s  (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum  c2)
    (or-gate c1 c2 c-out)
    'ok))

;;
;; (get-signal <wire>)
;; returns the current value of the signal on the wire
;;
;; (set-signal! <wire> <new value>)
;; changes the value of the signal on the wire to the new value
;;
;; (add-action! <wire> <procedure of no arguments>)
;; asserts that the designated procedure should be run whenever the
;; signal on the wire changes value
;;
;; (after-delay <tau> <procedure of no arguments>)
;; executes the procedure after delay tau

(define (inverter input output)
  (define (invert-output)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "invalid signal in logical-not" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value (logical-and (get-signal a1)
                                  (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure) ;; whichever of both changes,
  (add-action! a2 and-action-procedure) ;; we have to run the proc
  'ok)

(define (logical-and a b)
  (if (= 0 (* a b)) 0 1))


;;
;; exercise 3.28
;;

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value (logical-or (get-signal a1)
                                 (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda () (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure) ;; whichever of both changes,
  (add-action! a2 or-action-procedure) ;; we have to run the proc
  'ok)

(define (logical-or a b)
  (if (= 0 (+ a b)) 0 1))


;;
;; exercise 3.29
;;

(define (or-gate input1 input2 output)
  (let ((j1 (make-wire))
        (j2 (make-wire))
        (o  (make-wire)))
    (inverter input1 j1)
    (inverter input2 j2)
    (and-gate j1 j2 o)
    (inverter o output)))

;;
;; the delay would be equal to the sum of and-gate's and two times the
;; inverter's delays
;;


;;
;; exercise 3.30
;;

;; (define (full-adder a b c-in sum c-out)

;;
;; suppose there are no a, b, and s wires, just s, then the solution
;; for n full-adders would be

(define (ripple-carry-adder n c-out)
  (if (= n 0)
    (set-signal! c-out 0)
    (let ((c-in (make-wire)))
      (go (- n 1) c-in)
      (ripple-carry-adder c-in c-out))))
    
;;
;; the actual solution
;;
(define (ripple-carry-adder a b s c-out)
  """ assumes that a, b, and s are all of the same length """
  (if (null? a)
    (set-signal! c-out 0)
    (let ((c-in (make-wire)))
      (ripple-carry-adder (cdr a) (cdr b)      (cdr s) c-in)
      (full-adder         (car a) (car b) c-in (car s) c-out))))

;;
;; Representing wires
;;

(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    ;;
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin
          (set! signal-value new-value)
          (call-each action-procedures))
        'done))
    ;;
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    ;;
    (define (dispatch m)
      (cond ((eq? m 'get-signal ) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation: WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal  wire) (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

;;
;; The agenda
;;

;;
;; (make-agenda) returns a new empty agenda
;;
;; (empty-agenda? <agenda>) is true iff the specified agenda is empty
;;
;; (first-agenda-item <agenda>) returns the first item on the agenda
;;
;; (remove-first-agenda-item! <agenda>) modifies the agenda by
;; removing the first item
;;
;; (add-to-agenda! <time> <action> <agenda>) motifies the agenda by
;; adding the given action procedure to be run an the specified time
;;
;; (current-time <agenda>) returns the current simulation time
;;
;; "the-agenda" is the name of our agenda variable
;;

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagage))))

;;
;; A sample simulation
;;

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name) (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire)))))

;; (define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define  or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum     (make-wire))
(define carry   (make-wire))

;; (probe 'sum   sum)
;; (probe 'carry carry)

;;
;; exercise 3.31
;;

;;
;; the call to function is needed to propagate the initial state on
;; the input wires
;;


;;
;; Implementing the agenda
;;

(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s)
  (car s))
(define (segment-queue s)
  (cdr s))

(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda)
  (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda)
  (car (segments agenda)))
(define (rest-segments agenda)
  (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr! segments (cons (make-new-time-segment time action)
                                   (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments! agenda (cons (make-new-time-segment time action)
                                  segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
      (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-time agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty: FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-current-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))



;;
;; exercise 3.32
;;

;;
;; that's just the preservation of the direction of the time arrow
;;

;;
;; 3.3.5 Propagation of Constraints
;;

;;
;; basic operations:
;;
;; (has-value? <connector>) tell whether the connector has a value
;;
;; (get-value <connector>) returns the connector's current value
;;
;; (set-value! <connector> <new-value> <informant>) indicates that the
;; informant is requesting the connector to set its value to the new
;; value
;;
;; (forget-value! <connector> <retractor>) tells the connector that
;; the retractor is requesting it to forget its value
;;
;; (connect <connector> <new-constraint>) tells the connector to
;; participate in the new constraint
;;
;; (inform-about-value) tells the given constraint that the connector
;; has a value
;;
;; (inform-about-no-value) tells the constraint that the connector has
;; lost its value
;;


(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum (+ (get-value a1) (get-value a2)) me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2 (- (get-value sum) (get-value a1)) me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1 (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    ;; first try to drop everything which was set by me
    (forget-value! sum me)
    (forget-value! a1  me)
    (forget-value! a2  me)
    ;; in case there are values left, propagate them
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: ADDER" request))))
  (connect a1  me)
  (connect a2  me)
  (connect sum me)
  me)

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))


(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product (* (get-value m1) (get-value m2)) me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2 (/ (get-value product) (get-value m1)) me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1 (/ (get-value product) (get-value m2)) me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1      me)
    (forget-value! m2      me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value) (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: MULTIPLIER" request))))
  (connect m1      me)
  (connect m2      me)
  (connect product me)
  me)



(define (constant value connector)
  (define (me request)
    (error "Unknown request: CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)


(define (probe name connector)
  (define (print-probe value)
    (newline) (dipslay "Proble: ") (display name)
    (display " = ") (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)  (process-new-value))
          ((eq? request 'I-lost-my-value) (process-forget-value))
          (else (error "Unknown request: PROBE" request))))
  (connect connector me)
  me)

;;
;; Representing connectors
;;

(define (make-connector)
  (let ((value #f) (informant #f) (constraint '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter inform-about-value constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
        (begin
          (set! informant #f)
          (for-each-except retractor inform-about-no-value constraints))
        'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
        (set! constraints (cons new-constraint constraints)))
      (if (has-value? me)
        (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?) (if informant #t #f))
            ((eq? request 'value)       value)
            ((eq? request 'set-value!)  set-my-value)
            ((eq? request 'forget)      forget-my-value)
            ((eq? request 'connect)     connect)
            (else (error "unknown operation: CONNECTOR" request))))
    me))

(define (for-each-except exception procedure list)
  (define (loop items)
    (cond ((null? items) 'done)
          ((eq? (car items) exception) (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

(define (has-value? connector) (connector 'has-value?))
(define (get-value  connector) (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'conenct) new-constraint))

;;
;; exercise 3.33
;;

(define (averager a b c)
  (let ((u (make-connector))
        (v (make-connector)))
    (adder a b u)
    (multiplier u v c)
    (constant 0.5 v)))

