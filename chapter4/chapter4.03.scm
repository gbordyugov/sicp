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
;;         ((null? (cdr items) true))
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

(define (require p) (if (not p) (amb)))
(define (multiple-dwelling)
  (let ((cooper   (amb   2 3 4 5))
        (miller   (amb 1 2 3 4 5)))
    (require (> miller cooper))
    (let ((fletcher (amb 2 3 4)))
      (require (not (= (abs (- fletcher cooper)) 1)))
      (let ((smith (amb 1 2 3 4 5)))
        (require (not (= (abs (- smith  fletcher)) 1)))
        (let ((baker (amb 1 2 3 4 5)))
          (require (distinct? (list baker cooper fletcher miller smith)))
          (list (list 'baker    baker)
                (list 'cooper   cooper)
                (list 'fletcher fletcher)
                (list 'miller   miller)
                (list 'smith    smith)))))))


;;
;; exercise 4.41
;;


(define (multiple-dwelling)
  (define (distinct? items)
    (cond ((null? items) true)
          ((null? (cdr items)) true)
          ((member (car items) (cdr items)) false)
          (else (distinct? (cdr items)))))
  (define (check baker cooper fletcher miller smith)
    (and (distinct? (list baker cooper fletcher miller smith))
         (not (= baker    5))
         (not (= cooper   1))
         (not (= fletcher 5))
         (not (= fletcher 1))
         (> miller cooper)
         (not (= (abs (- smith  fletcher)) 1))
         (not (= (abs (- fletcher cooper)) 1))))
  (let ((rooms (list 1 2 3 4 5)))
    (map (lambda (baker)
           (map (lambda (cooper)
                  (map (lambda (fletcher)
                         (map (lambda (miller)
                                (map (lambda (smith)
                                       (if (check baker cooper fletcher miller smith)
                                         (begin
                                           (newline)
                                           (display (list baker cooper fletcher miller smith)))))
                                     rooms))
                              rooms))
                       rooms))
                rooms))
         rooms)
    'ok))


;;
;; exercise 4.42
;;
(load "amb-interpreter.scm")
(define (require p) (if (not p) (amb)))
(define (five-liars)
  (define (xor a b)
    (if a
      (if b false true)
      (if b true false)))
  (let ((b (amb 1 2 3 4 5))
        (e (amb 1 2 3 4 5))
        (j (amb 1 2 3 4 5))
        (k (amb 1 2 3 4 5))
        (m (amb 1 2 3 4 5)))
    (require (distinct? (list b e j k m)))
    (require (xor (= k 2) (= b 3)))
    (require (xor (= e 1) (= j 2)))
    (require (xor (= j 3) (= e 5)))
    (require (xor (= k 2) (= m 4)))
    (require (xor (= m 4) (= b 1)))
    (list (list 'b b)
          (list 'e e)
          (list 'j j)
          (list 'k k)
          (list 'm m))))

;;
;; exercise 4.43
;;

(define (require p) (if (not p) (amb)))
(define (yachts)
  (let ((mary-ann  (amb 'moore))
        (gabrielle (amb        'downing 'hall 'parker))
        (lorna     (amb        'downing 'hall 'parker))
        (rosalind  (amb        'downing 'hall 'parker))
        (melissa   (amb 'hood)))
    (require (distinct? (list mary-ann gabrielle lorna rosalind melissa)))
    (require (not (eq? 'hood  gabrielle)))
    (require (not (eq? 'moore lorna    )))
    (require (not (eq? 'hall  rosalind )))
    (require      (eq? 'hood  melissa   ))
    (require
      ;; this is a hack I borrowed from Eli Bendersky
      (cond ((eq? gabrielle 'downing) (eq? melissa 'parker))
            ((eq? gabrielle 'hall)    (eq? rosalind 'parker))
            (else false)))
    (list (list 'mary-ann  mary-ann)
          (list 'gabrielle gabrielle)
          (list 'lorna     lorna)
          (list 'rosalind  rosalind)
          (list 'melissa   melissa))))

;;
;; exercise 4.44
;;

(define (attacks? x1 y1 x2 y2)
  (cond ((= x1 x2)                           true)
        ((= y1 y2)                           true)
        ((= (abs (- x1 x2)) (abs (- y1 y2))) true)
        (else false)))

(define (enumerate items)
  (define (go items n acc)
    (if (null? items)
      acc
      (go (cdr items) (+ n 1) (cons n acc))))
  (go items 1 '()))

(enumerate '(a b c d e f))

(define (zip-with op a b)
  (define (go a b acc)
    (if (or (null? a) (null? b))
      acc
      (go (cdr a) (cdr b) (cons (op (car a) (car b)) acc))))
  (define (reverse lst)
    (define (go lst acc)
      (if (null? lst)
        acc
        (go (cdr lst) (cons (car lst) acc))))
      (go lst '()))
  (reverse (go a b '())))

(zip-with list '(1 2 3) '(a b c d))

(define (require p) (if (not p) (amb)))
(define (eight-queens)
  (define (attacks? x1 y1 x2 y2)
    (cond ((= x1 x2)                           true)
          ((= y1 y2)                           true)
          ((= (abs (- x1 x2)) (abs (- y1 y2))) true)
          (else false)))
  (define (this-ok? c r cs rs)
    (if (null? cs)
      true
      (if (attacks? c r (car cs) (car rs))
        false
        (this-ok? c r (cdr cs) (cdr rs)))))
  (define (ok-position? columns rows)
    (if (null? columns)
      true
      (let ((rest-columns (cdr columns))
            (rest-rows    (cdr rows)))
        (if (not (ok-position? rest-columns rest-rows))
          false
          (let ((this-column (car columns))
                (this-row    (car rows)))
            (this-ok? this-column this-row rest-columns rest-rows))))))
  (let ((q1 (amb 1 2 3 4 5 6 7 8)))
    (let ((q2 (amb 1 2 3 4 5 6 7 8)))
      (require (distinct? (list q1 q2)))
      (let ((q3 (amb 1 2 3 4 5 6 7 8)))
        (require (distinct? (list q1 q2 q3)))
        (let ((q4 (amb 1 2 3 4 5 6 7 8)))
          (require (distinct? (list q1 q2 q3 q4)))
          (let ((q5 (amb 1 2 3 4 5 6 7 8)))
            (require (distinct? (list q1 q2 q3 q4 q5)))
            (let ((q6 (amb 1 2 3 4 5 6 7 8)))
              (require (distinct? (list q1 q2 q3 q4 q5 q6)))
              (let ((q7 (amb 1 2 3 4 5 6 7 8)))
                (require (distinct? (list q1 q2 q3 q4 q5 q6 q7)))
                (let ((q8 (amb 1 2 3 4 5 6 7 8)))
                  (require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
                  (let ((columns (list q1 q2 q3 q4 q5 q6 q7 q8))
                        (rows    (list  1  2  3  4  5  6  7  8)))
                    (require (distinct? (list q1 q2 q3 q4 q5 q6 q7 q8)))
                    (require (ok-position? columns rows))
                    (list q1 q2 q3 q4 q5 q6 q7 q8)))))))))))

;;
;; prototype of the functions above
;;
(define (ok-position? columns rows)
  (define (this-ok? c r cs rs)
    (define (attacks? x1 y1 x2 y2)
      (cond ((= x1 x2)                           true)
            ((= y1 y2)                           true)
            ((= (abs (- x1 x2)) (abs (- y1 y2))) true)
            (else false)))
    (if (null? cs)
      true
      (if (attacks? c r (car cs) (car rs))
        false
        (this-ok? c r (cdr cs) (cdr rs)))))
  (if (null? columns)
    true
    (let ((rest-columns (cdr columns))
          (rest-rows    (cdr rows)))
      (if (not (ok-position? rest-columns rest-rows))
        false
        (let ((this-column (car columns))
              (this-row    (car rows)))
          (this-ok? this-column this-row rest-columns rest-rows))))))

(ok-position? '(1 3) '(3 2))

;;
;; exercise 4.44
;;
;; new approach
;;

(define (require p) (if (not p) (amb)))

(define (and a b)
  (if a
    (if b true false)
    false))

(define (safe-horizontal? q rest)
  (not (member q rest)))

(define (safe-diagonal? q rest)
  (define (go q counter rest)
    (if (null? rest)
      true
      (and (not (= (abs (- q (car rest)))
                   counter))
           (go q (+ 1 counter) (cdr rest)))))
  (go q 1 rest))

(define (safe? positions)
  (if (null? positions)
    true
    (let ((queen (car positions))
          (rest  (cdr positions)))
      (and (safe-horizontal? queen rest)
           (  safe-diagonal? queen rest)))))

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(define (list-amb items)
  (if (null? items)
    (amb)
    (amb (car items) (list-amb (cdr items)))))

(define (queens board-size)
  (define (queens-iter k)
    (if (= k 0)
      '()
      (let ((pos (list-amb (enumerate-interval 1 board-size)))
            (prev (queens-iter (- k 1))))
        (require (safe? (cons pos prev)))
        (cons pos prev))))
  (queens-iter board-size))

