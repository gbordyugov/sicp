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

;; (what 4)

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; (prime-sum? '(1 4))

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

;; (prime-sum-pairs 13)

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

;; (prime-sum-pairs 13)


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


;;
;; execise 2.43
;;

(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
      (list empty-board)
      (filter
        (lambda (positions) (safe? k positions))
        (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
        (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(queens-slow 6)

;;
;; why the above code is slow?
;;
;; the above code is slow because the recursive call (queen-cols (- k
;; 1)) is within the outer loop over the position of the new queen,
;; thus unnecessary keeping producing the same result for each new
;; position of the additional queen
;;
;; how long will it take the slow version to solve the eight-queens
;; puzzle, if the fast version solves the puzzle in time T?
;;
;; I still don't know ;-)
;;


;;
;; the Painter Language
;;


;; beside :: Painter -> Painter -> Painter
(define (beside left  right) undefined)

;; below :: Painter -> Painter -> Painter
(define (below  lower upper) undefined)

;; wave :: Painter

;; wave2 :: Painter
;; (define wave2 (beside wave (flip-vert wave)))
;; wave4 :: Painter
;; (define wave4 (below wave2 wave2))

;; flipped-pairs :: Painter -> Painter
(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))

;; wave4 :: Painter
;; (define wave4 (flipped-pairs wave))

;; right-split :: Painter -> Int -> Painter
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

;; corner-split :: Painter -> Int -> Painter
(define (corner-split painter n)
  (if (= n 0)
    painter
    (let ((up    (up-split painter (- n 1)))
          (right (right-split painter (- n 1))))
      (let ((top-left (beside up up))
            (bottom-right (below right right))
            (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
                (below bottom-right corner))))))

;; square-limit :: Painter -> Int -> Painter
(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))


;;
;; Exercise 2.44
;;

;; up-split :: Painter -> Int -> Painter
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))


;;
;; continuing with combinators
;;

;; square-of-four :: (Painter -> Painter) -- tl
;;                -> (Painter -> Painter) -- tr
;;                -> (Painter -> Painter) -- bl
;;                -> (Painter -> Painter) -- br
;;                -> (Painter -> Painter) -- resulting map
(define (square-of-four tl tr bl br)
  (lambda (painter) ;; :: Painter -> Painter
    (let ((top    (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

;; flipped-pairs :: Painter -> Painter
(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

;; square-limit :: Painter -> Painter
(define (square-limit painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;;
;; exercise 2.45
;;

;;
;; the old definitions...
;;

;; right-split :: Painter -> Int -> Painter
(define (right-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (right-split painter (- n 1))))
      (beside painter (below smaller smaller)))))

;; up-split :: Painter -> Int -> Painter
(define (up-split painter n)
  (if (= n 0)
    painter
    (let ((smaller (up-split painter (- n 1))))
      (below painter (beside smaller smaller)))))

;; ...by using...
;; split :: (Painter -> Painter)
;;       -> (Painter -> Painter)
;;       -> (Painter -> Int -> Painter)
(define (split a b)
  (lambda (painter n) ;; :: Painter -> Int -> Painter
    (if (= 0 n)
      painter
      (let ((smaller ((split a b) painter (- n 1))))
        (a painter (b smaller smaller))))))
;; that seems to be a valid solution to exercise 2.45!

;; ...should be expressed as...
;; (define right-split (split beside below ))
;; (define up-split    (split below  beside))


;;
;; Frames
;;

;;
;; frame coordinate map is an affine transformation
;; [0, 1] x [0, 1] -> R^2
;; type FrameCoordinateMap = Vector -> Vector

;; frame-coord-map :: Frame -> FrameCoordinateMap
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
      (origin-frame frame)
      (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                (scale-vect (ycor-vect v) (edge2-frame frame))))))


;;
;; exercise 2.46
;;

;; make-vect :: Double -> Double -> Vector
(define make-vect cons)

;; xcor-vect :: Vector -> Double
(define xcor-vect car)

;; ycor-vect :: Vector -> Double
(define ycor-vect cdr)

;; add-vect :: Vector -> Vector -> Vector
(define (add-vect a b)
  (make-vect (+ (xcor-vect a) (xcor-vect b)
                (ycor-vect a) (ycor-vect b))))

;; sub-vect :: Vector -> Vector -> Vector
(define (sub-vect a b)
  (make-vect (- (xcor-vect a) (xcor-vect b)
                (ycor-vect a) (ycor-vect b))))

;; scale-vect :: Vector -> Double -> Vector
(define (scale-vect v s)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))

;;
;; exercise 2.47
;;

;;
;; first representation
;;

;; make-frame :: Vector -- origin
;;            -> Vector -- edge1
;;            -> Vector -- edge2
;;            -> Frame
(define (make-frame o e1 e2)
  (list o e1 e2))

;; origin-frame :: Frame -> Vector
(define (origin-frame f)
  (car f))

;;  edge1-frame :: Frame -> Vector
(define (edge1-frame f)
  (car (cdr f)))

;;  edge2-frame :: Frame -> Vector
(define (edge2-frame f)
  (car (cdr (cdr f))))



;;
;; second representation
;;

;; make-frame :: Vector -- origin
;;            -> Vector -- edge1
;;            -> Vector -- edge2
;;            -> Frame
(define (make-frame o e1 e2)
  (cons o (cons e1 e2)))

;; origin-frame :: Frame -> Vector
(define (origin-frame f)
  (car f))

;;  edge1-frame :: Frame -> Vector
(define (edge1-frame f)
  (car (cdr f)))

;;  edge2-frame :: Frame -> Vector
(define (edge2-frame f)
  (cdr (cdr f)))


;;
;; Painters
;;

;; type Painter = Frame -> Image (whatever Image is, might be IO as
;; well)

;; segments->painter :: [Segment] -> Painter
(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
      (lambda (segment)
        (draw-line ((frame-coord-map frame) (start-segment segment))
                   ((frame-coord-map frame) (  end-segment segment))))
      sigment-list)))


;;
;; exercise 2.48
;;

;; make-segment :: Vector -> Vector -> Segment
(define (make-segment start end)
  (cons start end))

;; start-segment : Segment -> Vector
(define start-segment car)

;; end-segment : Segment -> Vector
(define end-segment cdr)


;;
;; exercise 2.49
;;

;; a)
;; frame-outline :: Frame -> Painter
(define (frame-outline f)
  (let ((p1 (origin-frame f))
        (p2 (edge1-frame f))
        (p3 (add-vect (edge1-frame f)
                      (edge2-frame f)))
        (p4 (edge2-frame f)))
    (let ((s1 (make-segment p1 p2))
          (s2 (make-segment p2 p3))
          (s3 (make-segment p3 p4))
          (s4 (make-segment p4 p1)))
      (segments->painter (list s1 s2 s3 s4)))))

;; b)
;; frame-x :: Frame -> Painter
(define (frame-x f)
  (let ((p1 (origin-frame f))
        (p2 (edge1-frame f))
        (p3 (add-vect (edge1-frame f)
                      (edge2-frame f)))
        (p4 (edge2-frame f)))
    (let ((s1 (make-segment p1 p3))
          (s2 (make-segment p2 p4)))
      (segments->painter (list s1 s2)))))

;; c)
;; frame-outline :: Frame -> Painter
(define (frame-outline f)
  ;; midpoint :: Segment -> Vector
  (define (midpoint s)
    (let ((v1 (start-segment s))
          (v2 (  end-segment s)))
      (scale-vect (add-vect v1 v1) 0.5)))
  (let ((p1 (origin-frame f))
        (p2 (edge1-frame f))
        (p3 (add-vect (edge1-frame f)
                      (edge2-frame f)))
        (p4 (edge2-frame f)))
    (let ((s1 (make-segment p1 p2))
          (s2 (make-segment p2 p3))
          (s3 (make-segment p3 p4))
          (s4 (make-segment p4 p1)))
      (let ((m1 (midpoint s1))
            (m2 (midpoint s2))
            (m3 (midpoint s3))
            (m4 (midpoint s4)))
        (let ((s1 (make-segment m1 m2))
              (s2 (make-segment m2 m3))
              (s3 (make-segment m3 m4))
              (s4 (make-segment m4 m1)))
      (segments->painter (list s1 s2 s3 s4)))))))

;; d) skipped


;;
;; transformation of painters
;;

;; transform-painter :: Painter
;;                   -> Vector -> Vector -> Vector
;;                   -> Painter (= Frame -> Draw)
(define (transform-painter painter origin corner1 corner2)
  (lambda (frame) ;; the new painter is a function of Frame
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame new-origin
                             (sub-vect (m corner 1) new-origin)
                             (sub-vect (m corner 2) new-origin)))))))

;; flip-vect :: Painter -> Painter
(define (flip-vect painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;; shrink-to-upper-right :: Painter -> Painter
(define (shrink-to-upper-right painter)
  (trainsform-painter painter
                      (make-vect 0.5 0.5)
                      (make-vect 1.0 0.5)
                      (make-vect 0.5 1.0)))

;; rotate90 :: Painter -> Painter
(define (rotate90 painter)
  (trainsform-painter painter
                      (make-vect 1.0 0.0)
                      (make-vect 1.0 1.0)
                      (make-vect 0.5 0.0)))

;; squash-inwards :: Painter -> Painter
(define (squash-inwards painter)
  (trainsform-painter painter
                      (make-vect 0.0  0.0)
                      (make-vect 0.65 0.35)
                      (make-vect 0.35 0.65)))

;; beside :: Painter -> Painter -> Painter
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left ;; :: Painter
            (transform-painter painter1
                               (make-vect 0.0 0.0)
                               split-point
                               (make-vect 0.0 1.0)))
          (paint-right ;; :: Painter
            (transform-painter painter2
                               (make-vect 1.0 0.0)
                               split-point
                               (make-vect 0.5 1.0))))
      (lambda (frame) ;; :: Frame -> Draw ( = Painter)
        (paint-left frame)
        (paint-right frame)))))

;;
;; exercise 2.50
;;

;; a)
;; flip-horiz :: Painter -> Painter
(define (flip-horiz painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; b)
;; rotate180 :: Painter -> Painter
(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.0)))

;; c)
;; rotate270 :: Painter -> Painter
(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;;
;; exercise 2.51
;;
;; a)
;; solution analogous to below above
;; below :: Painter -> Painter -> Painter
(define (beside painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-upper ;; :: Painter
            (transform-painter painter1
                               split-point
                               (make-vect 1.0 0.0)
                               (make-vect 0.0 1.0)))
          (paint-lower ;; :: Painter
            (transform-painter painter2
                               (make-vect 0.0 0.0)
                               (make-vect 1.0 0.0)
                               split-point)))
      (lambda (frame) ;; :: Frame -> Draw ( = Painter)
        (paint-upper frame)
        (paint-lower frame)))))

;; b)
;; solution by rotations
;; below :: Painter -> Painter -> Painter
(define (beside p1 p2)
  (let ((s1 (rotate90 painter1))
        (s2 (rotate90 painter2)))
    (let ((stacked (below s1 s2)))
      (let ((rotated (rotate270 stacked)))
        (lambda (frame)
          (rotated frame))))))


;;
;; exercise 2.52 skipped
;;
