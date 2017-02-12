(define (list-ref items n)
  (if (= 0 n)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (glength items)
  (if (null? items)
    0
    (+ 1 (glength (cdr items)))))

(define odds (list 1 3 5 7))

(glength odds)

(define (glength items)
  (define (go a count)
    (if (null? a)
      count
      (go (cdr a) (+ 1 count))))
  (go items 0))

(append squares odds)

(append odds squares)

(define (gappend list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))

;;
;; exercise 2.17
;;

(define (last-pair l)
  (if (null? (cdr l))
    l
    (last-pair (cdr l))))

(last-pair squares)
(last-pair odds)

;;
;; exercise 2.18
;;

(define (greverse l)
  (define (go l acc)
    (if (null? l)
      acc
      (go (cdr l) (cons (car l) acc))))
  (go l '()))

(greverse (list 1 2 3))


;;
;; exercise 2.19
;;

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (define (no-more? coins) (null? coins))
  (define first-denomination car)
  (define except-first-denomination cdr)
  (cond
    ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0)
    (else
      (+ (cc amount (except-first-denomination coin-values))
         (cc (- amount (first-denomination coin-values)) coin-values)))))
        
(cc 10 us-coins)
(cc 10 uk-coins)


;;
;; exercise 2.20
;;

(define (same-parity first . rest)
  (define (go rest acc) ;; tail-recursive yeah!
    (cond
      ((null? rest) acc)
      ((= (remainder (+ (car rest) (car acc)) 2) 0)
       (go (cdr rest)(cons (car rest) acc)))
      (else (go (cdr rest) acc))))
  (reverse (go rest (list first))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7 8)


;;
;; exercise 2.21
;;

(define (square-list-1 items)
  (if (null? items)
    '()
    (cons (* (car items) (car items))
          (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x)) items))

(square-list-1 (list 1 2 3 4 5 6))
(square-list-2 (list 1 2 3 4 5 6))

;;
;; exercise 2.22
;;

(define (square-list-3 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons (square (car things)) answer))))
  (iter items '()))

;; of course it would reverse the list!

(square-list-3 (list 1 2 3 4 5 6))

(define (square-list-4 items)
  (define (iter things answer)
    (if (null? things)
      answer
      (iter (cdr things)
            (cons answer (square (car things))))))
  (iter items '()))

;; this results in an anti-list, i.e. list made of conses with cars
;; pointing at the rest of the list
(square-list-4 (list 1 2 3 4 5 6))


;;
;; exercise 2.23
;;

(define (for-each f list)
  (map f list)
  #t)

(for-each (lambda (x) (newline) (display x)) (list 1 2 3))


;;
;; count-leaves
;;

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;;
;; exercise 2.24
;;

;; just a sketch

;;
;; exercise 2.25
;;

(car (cdr (car (cdr (cdr (list 1 3 (list 5 7) 9))))))
(car (car '((7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '(1 (2 (3 (4 (5 (6 7))))))))))))))))))

;;
;; exercise 2.26
;;

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
(cons x y)
(list x y)

;;
;; exercise 2.27
;;

(define (deep-reverse l)
  (define (go l acc)
    (cond
      ((null? l) acc)
      ((not (pair? (car l))) (go (cdr l) (cons (car l) acc)))
      (else (go (cdr l) (cons (go (car l) '()) acc)))))
  (go l '()))

(deep-reverse '(1 2 3 (4 5)))
(deep-reverse '((1 2) (3 4)))

;;
;; exercise 2.28
;;
;; that's basically the function ``flatten'' from ``On Lisp''
;;

(define (fringe l)
  (define (go l acc)
    (cond
      ((null? l) acc)
      ((not (pair? l)) (cons l acc))
      (else (go (car l) (go (cdr l) acc)))))
  (go l '()))

(fringe '(1 2 3 4 5))

(fringe '((1 2 (3 4)) (1 2) (3 4) 5))

;;
;; exercise 2.29
;;

(define (make-mobile left-branch right-branch)
  (list left-branch right-branch))

(define (make-branch length structure)
  (list length structure))

;; b.
(define ( left-branch mobile) (car      mobile))
(define (right-branch mobile) (car (cdr mobile)))

(define (branch-length    branch) (car      branch))
(define (branch-structure branch) (car (cdr branch)))



;; b.
(define (branch-weight branch)
  (let ((s (branch-structure branch)))
   (if (number? s) s (mobile-weight s))))
    
(define (mobile-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
  
(define (total-weight mobile)
  (mobile-weight mobile))

(define leaf1 (make-branch 1.0 2.0))
(define leaf2 (make-branch 3.0 4.0))
(define leaf3 (make-branch 5.0 6.0))

(define mobile1 (make-mobile leaf1   leaf2))
(define branch1 (make-branch 3.0     mobile1))
(define mobile  (make-mobile branch1 leaf3))

(left-branch  mobile)
(right-branch mobile)

(total-weight mobile)

;; c.
(define (branch-torgue b)
  (let ((s (branch-structure b))
        (l (branch-length    b)))
   (if (number? s) (* l s) (* l (mobile-weight s)))))
    
(define (branch-balanced? b)
  (let ((s (branch-structure b)))
   (if (number? s) #t (mobile-balanced? s))))

(branch-balanced? (make-branch 1.0 3.0))

(define (mobile-balanced? m)
  (let* ((lb ( left-branch m))
         (rb (right-branch m))
         (tt (= (branch-torgue lb) (branch-torgue rb))))
    (and tt (branch-balanced? lb) (branch-balanced? rb))))

(mobile-balanced? (make-mobile leaf1 leaf1))
(mobile-balanced? (make-mobile leaf1 leaf2))
(define mobile2 (make-mobile leaf1 leaf1))
(define branch2 (make-branch 3.0   mobile2))
(mobile-balanced? (make-mobile branch2 branch2))

;; d.
(define (make-mobile left-branch right-branch)
  (cons left-branch right-branch))

(define (make-branch length structure)
  (cons length structure))

(define ( left-branch mobile) (car mobile))
(define (right-branch mobile) (cdr mobile))

(define (branch-length    branch) (car branch))
(define (branch-structure branch) (cdr branch))


;;
;; exercise 2.30
;;

(define (square-tree-1 tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (* tree tree))
        (else (cons (square-tree-1 (car tree))
                    (square-tree-1 (cdr tree))))))

(define (square-tree-2 tree)
  (map (lambda (x)
         (if (pair? x)
           (square-tree-2 x)
           (* x x)))
       tree))

(square-tree-1 '((1 2) (3 4) 5))

(square-tree-2 '((1 2) (3 4) 5))

;;
;; exercise 2.30
;;

(define (tree-map-1 f tree)
  (map (lambda (x)
         (if (pair? x)
           (tree-map-1 f x)
           (f x)))
       tree))

(define (tree-map-2 f tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (f tree))
        (else (cons (tree-map-2 f (car tree))
                    (tree-map-2 f (cdr tree))))))

(define (square-tree-3 tree) (tree-map-1 (lambda (x) (* x x)) tree))
(define (square-tree-4 tree) (tree-map-2 (lambda (x) (* x x)) tree))

(square-tree-3 '((1 2) (3 4) 5))
(square-tree-4 '((1 2) (3 4) 5))


;;
;; exercise 2.32
;;

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((rest (subsets (cdr s))))
     (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets '(1 2 3 4 5 6 7))


;;
;; exercise 2.23
;;

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence) (accumulate op initial (cdr sequence)))))

(accumulate cons '() '(1 2 3))
(accumulate + 0 '(1 2 3))

(define (g-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))

(g-map (lambda (x) (+ 3 x)) '(1 2 3))

(define (g-append seq1 seq2) (accumulate cons seq1 seq2))

(g-append '() '())
(g-append '(1 2 3) '(1 2 3))

(define (g-length sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(g-length '())
(g-length '(1 2 3))


;;
;; exercise 2.34
;;

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

(horner-eval 1 '(1 2 3))


;;
;; exercise 2.34
;;

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(count-leaves '((1 2) 3 (4 5)))


(define (new-count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                           (new-count-leaves x)
                           1))
                       t)))

(new-count-leaves '(1))

(new-count-leaves '((1 2) 3 ((3 4 (5 6)) 4 5)))


;;
;; exercise 2.36
;;

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    '()
    (cons (accumulate   op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))


;;
;; exercise 2.37
;;

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m column)
  (map (lambda (row) (dot-product row column)) m))

(matrix-*-vector '((1 2) (3 4)) '(1 1))

(define (transpose mat)
  (accumulate-n cons '() mat))

(transpose '((1 2) (3 4)))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
   (map (lambda (column) (matrix-*-vector m column)) m)))

(matrix-*-matrix '((1 1) (1 1)) '((1 1) (1 1)))


;;
;; exercise 2.38
;;

(define fold-r accumulate)

(define (fold-l op initial sequence)
  (define (iter result rest)
    (if (null? rest)
      result
      (iter (op result (car rest))
            (cdr rest))))
      (iter initial sequence))


(fold-r / 1 '(1 2 3))

(fold-l / 1 '(1 2 3))

(fold-r list '() '(1 2 3))

(fold-l list '() '(1 2 3))

;; (op x y) = (op y x)


;;
;; exercise 2.39
;;

(define (g-reverse-r sequence)
  (fold-right (lambda (x y) (append y (list x))) '() sequence))

(g-reverse-r '(1 2 3))


(define (g-reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) '() sequence))

(g-reverse-l '(1 2 3))


;;
;; preparation to exercise 2.40
;;

(define (enumerate-interval low high)
  (if (> low high)
    '()
    (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 2 7)

(define (what n)
  (accumulate append '()
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))

(what 4)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(prime-sum? '(1 4))

(define (make-pair-sum pair)
  (let ((a (car pair))
        (b (cadr pair)))
    (list a b (+ a b))))

(make-pair-sum '(1 4))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (flatmap
                            (lambda (i)
                              (map (lambda (j) (list i j))
                                   (enumerate-interval 1 (- i 1))))
                            (enumerate-interval 1 n)))))

(prime-sum-pairs 13)

(define (permutations s)
  (if (null? s)
    (list '())
    (flatmap (lambda (x)
               (let ((s-x (remove- x s)))
                (map (lambda (p) (cons x p))
                     (permutations s-x))))
             s)))

(define (remove- item sequence)
  (filter (lambda (x) (not (= x item))) sequence))

(permutations '(1 2 3 4))

;;
;; exercise 2.40
;;

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
             (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(unique-pairs 3)

(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 13)


;;
;; exercise 2.41
;;

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (pair) (cons i pair))
                    (unique-pairs (- i 1))))
           (enumerate-interval 1 n)))

(unique-triples 4)
             
  
(define (sum-triples n s)
  (filter (lambda (triple)
            (= (+ (car   triple)
                  (cadr  triple)
                  (caddr triple))
               s))
          (unique-triples n)))

(sum-triples 5 6)


;;
;; exercise 2.42 - the dreaded queens puzzle!
;;

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                     new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;
;; I represent positions (the first k columns of a board): (a b c d
;; ...) as a list of k numbers, representing the rows of k queens
;; (in the reverse order, i.e. a for the k-th queen, b for the
;; k-1st queen, etc.)
;;

;;
;; represent an empty board
;;
(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))
          
;;
;; check if queen in q-th row is not in the same row with any queens
;; from the rest
;;
(define (safe-horizontal? q rest)
  (not (member q rest)))

;;
;; check if queen in q-th row is not on the same diagonal with any
;; queens from the rest
;;
(define (safe-diagonal? q rest)
  (define (go q counter rest)
    (if (null? rest)
      #t
      (and (not (= (abs (- q (car rest)))
                   counter))
           (go q (+ 1 counter) (cdr rest)))))
  (go q 1 rest))

(safe-diagonal? 3 '())

(safe-diagonal? 3 '(3))

(safe-diagonal? 3 '(4))

(define (safe? k positions)
  (if (null? positions)
    #t
    (let ((queen (car positions))
          (rest  (cdr positions)))
      (and (safe-horizontal? queen rest)
           (  safe-diagonal? queen rest)))))

(safe? 8 '())

(safe? 8 '(1))

(safe? 8 '(1 2))

(safe? 8 '(1 3))

(safe? 8 '(1 1))

(queens 8)
