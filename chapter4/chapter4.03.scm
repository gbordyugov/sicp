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
