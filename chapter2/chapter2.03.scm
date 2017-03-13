(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

;;
;; exercise 2.53
;;

(list 'a 'b 'c)

(list (list 'george))

(cdr '((x1 x2) (y1 y2)))

(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))

(memq 'red '((red shoes) (blue socks)))

(memq 'red '(red shoes blue socks))

;;
;; exercise 2.54
;;

(define (gequal? a b)
  (cond ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b))
              (gequal? (cdr a) (cdr b))))
        ((or (pair? a) (pair? b)) #f)
        (else (eq? a b))))

(gequal? 'a 'a)

(gequal? 'a 'b)

(gequal? 'a '(a b))

(gequal? '(a b) '(a b))

;;
;; exercise 2.55
;;

(car ''abracadabra)
;; = (car '(quote abracadabra)))
;; = quote


;;
;; Symbolic differentiation
;;

;;
;; considered as provided
;;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
          (error "unknown expression type: DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum     a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) ( cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier   s) ( cadr s))
(define (multiplicand s) (caddr s))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;
;; exercise 2.56
;;

(define (base     exp) ( cadr exp))
(define (exponent exp) (caddr exp))

;; (define (make-exponentiation base exp) (list '** base exp))

(define (exponentiation? e)
  (eq? (car e) '**))

(define (make-exponentiation base exp)
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) base)
        ((and (number? base) (number? exp))
         (expt base exp))
        (else (list '** base exp))))

;;
;; exercise 2.57
;;

;;
;; old version
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list '+ a1 a2))))
;;
;; new version
;;

;;
;; a small helper funciton
;;
;; collect-numbers-and-symbols :: [NumberOrExpression]
;;                             -> ([Number], [Expression])
(define (collect-numbers-and-exprs l op)
  (list (apply op (filter number? l))
        (filter (lambda (x) (not (number? x))) l)))

(collect-numbers-and-exprs '(x 1 y 2 z 3) +)

(collect-numbers-and-exprs '(x 1 (* x 3) 2 z 3) +)

;; make-sum :: [NumberOrExpression] -> Expression
;; the output can be an Expression, not always a sum
(define (make-sum . summands)
  (let* ((ns (collect-numbers-and-exprs summands +))
         (number  (car  ns))
         (exprs (cadr ns)))
    (cond ((null? exprs) number)
          ((and (= 0 number) (null? (cdr exprs))) (car exprs))
          ((= 0 number) (cons '+ exprs))
          (else (cons '+ (cons number exprs))))))

(make-sum '())

(make-sum 0)

(make-sum 0 1 2 3)

(make-sum 'x 'y 'z)

(make-sum 'x 'y 'z '(+ a b))

(make-sum '(+ a b))

(make-sum 1 'x)

(make-sum 0 'x)

(make-sum 0 1 'z 2)

(make-sum 0 1 'x 3 'y 5 6)

(make-sum 0 1 'x 3 'y 5 'z 6)

;; addend :: Sum -> Expression
(define (addend s) ( cadr s))

(make-sum 'x 2 'y 3)

(addend (make-sum 'x 2 3))

(addend (make-sum 'x 2 'y 3))

;; augend :: Sum -> Expression
(define (augend s)
  (let ((ttail (cddr s)))
    (if (null? (cdr ttail))
      (car ttail)
      (apply make-sum ttail))))

(augend (make-sum 'x 2 3))

(augend (make-sum 'x 2 'y 3))

(augend (make-sum 'x 2 3))

(augend (make-sum 'x 2 '(* a b)))


;; make-sum :: [NumberOrExpression] -> Expression
;; the output can be an Expression, not always a sum
(define (make-product . factors)
  (let* ((ns (collect-numbers-and-exprs factors *))
         (number  (car  ns))
         (exprs (cadr ns)))
    (cond ((= 0 number) 0)
          ((null? exprs) number)
          ((and (= 1 number) (null? (cdr exprs))) (car exprs))
          ((= 1 number) (cons '* exprs))
          (else (cons '* (cons number exprs))))))

(make-product '())

(make-product 1)

(make-product 1 2 3)

(make-product 0 1 2 3)

(make-product 1 2 0 3)

(make-product 0 'x)

(make-product 0 'x '(+ a b))

(make-product 1 'x '(+ a b))

(make-product 1 'x)

(make-product 2 'x)

(make-product 1 1 'z 2)

(make-product 1 1 'x 3 'y 5 6)

(make-product 1 1 'x 3 'y 5 'z 6)

(make-product 1 1 'x 3 0 'y 5 'z 6)

;; multiplier :: Sum -> Expression
(define (multiplier s)
  (cadr s))

(make-product 'x 2 'y 3)

(multiplier (make-product 'x 2 3))

(multiplier (make-product 'x 2 'y 3))

(multiplier (make-product 'x 'y))

(multiplier (make-product 'x 'y 'z))

;; augend :: Sum -> Expression
(define (multiplicand s)
  (let ((ttail (cddr s)))
    (if (null? (cdr ttail))
      (car ttail)
      (apply make-product ttail))))

(multiplicand (make-sum 'x 2 3))

(multiplicand (make-product 'x 2 'y 3 '(* a b)))

(multiplicand (make-product 'x 2 3))

(multiplicand (make-product 'x 'y 'z '(- 3 4)))


;;
;; exercise 2.58 (a)
;;
;;
;; sum
;;
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (addend s) (  car s))
(define (augend s) (caddr s))
(define (sum? x) (and (pair? x)
                      (pair? (cdr x))
                      (pair? (cddr x))
                      (eq? (cadr x) '+)))

;;
;; product
;;
(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m1))))
(define (multiplier s)   (  car s))
(define (multiplicand s) (caddr s))
(define (product? x) (and (pair? x)
                          (pair? (cdr x))
                          (pair? (cddr x))
                          (eq? (cadr x) '*)))

;;
;; exercise 2.58 (b)
;;
;; seems to be a bit tougher than the previous one
;;
;; skipped for time being
;;

;;
;; Example: Representing Sets
;;

;;
;; Sets as unordered lists
;;
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
    set
    (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;;
;; exercise 2.59
;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((set3 (union-set (cdr set1) set2)))
                (if (element-of-set? (car set1) set3)
                  set3
                  (cons (car set1) set3))))))

(union-set '(4 5 1 2 3) '(1 4 5 2 6))

;;
;; exercise 2.60
;;
;; sets as lists with duplicates
;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(element-of-set? 1 '(1 2 3))

(element-of-set? 'a '(1 2 3))

;;
;; that one is fast
;;
(define adjoin-set cons)


;; that one is fast too
;;
(define union-set append)

;; this is just a repetition
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1) (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) (set2)))))

;;
;; sets as ordered lists
;;

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1))
          (x2 (car set2)))
      (cond ((= x1 x2)
             (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2) ;; x1 is not in set2
             (intersection-set (cdr set1) set2))
            ((< x2 x1) ;; x2 is not in set1
             (intersection-set set1 (cdr (set2))))))))

(intersection-set '(1 2 3) '(2 3 4))

;;
;; exercise 2.61
;;

(define (adjoin-set x set)
  (cond ((or (null? set) (< x (car set))) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 3 '(1 2 4 5))

(adjoin-set 3 '(1 2 3 4 5))

(adjoin-set 0 '(1 2 3 4 5))

(adjoin-set 6 '(1 2 3 4 5))

;;
;; todo - make it tail-recursive
;;


;;
;; exercise 2.62
;;

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x  (car set1))
                    (xs (cdr set1))
                    (y  (car set2))
                    (ys (cdr set2)))
                (cond ((= x y)
                       (cons x (union-set xs ys)))
                      ((< x y)
                       (cons x (union-set xs set2)))
                      ((< y x)
                       (cons y (union-set set1 ys))))))))

(union-set '(1 2 3) '(3 4 5))

;;
;; Set as binary trees
;;

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(adjoin-set 3 (make-tree 5 '() '()))


;;
;; exercise 2.63
;;

(define (tree->list-1 tree)
  (if (null? tree)
    '()
    (append (tree->list-1 (left-branch tree))
            (cons (entry tree)
                  (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
      result-list
      (copy-to-list (left-branch tree)
                    (cons (entry tree)
                          (copy-to-list (right-branch tree) result-list)))))
  (copy-to-list tree '()))

(define tree-a (make-tree 7
                           (make-tree 3
                                      (make-tree 1 
                                                 () 
                                                 ())
                                      (make-tree 5
                                                 '()
                                                 '()))
                           (make-tree 9
                                      '()
                                      (make-tree 11
                                                 '()
                                                 '()))))

(define tree-b (make-tree 3
                          (make-tree 1
                                     '()
                                     '())
                          (make-tree 7
                                     (make-tree 5
                                                '()
                                                '())
                                     (make-tree 9
                                                '()
                                                (make-tree 11
                                                           '()
                                                           '())))))

(define tree-c (make-tree 5
                          (make-tree 3
                                     (make-tree 1
                                                '()
                                                '())
                                     '())
                          (make-tree 9
                                     (make-tree 7
                                                '()
                                                '())
                                     (make-tree 11
                                                '()
                                                '()))))

(tree->list-1 tree-a)

(tree->list-2 tree-a)

(tree->list-1 tree-b)

(tree->list-2 tree-b)

(tree->list-1 tree-c)

(tree->list-2 tree-c)

;;
;; a) yes, they produce the same results
;;
;; b) no, one is tail recursive, and the other is just recursive

;;
;; exercise 2.64
;;

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
    (cons '() elts)
    (let ((left-size (quotient (- n 1) 2)))
      (let ((left-result (partial-tree elts left-size)))
        (let ((left-tree (car left-result))
              (non-left-elts (cdr left-result))
              (right-size (- n (+ left-size 1))))
          (let ((this-entry (car non-left-elts))
                (right-result (partial-tree (cdr non-left-elts)
                                            right-size)))
                (let ((right-tree (car right-result))
                      (remaining-elts (cdr right-result)))
                  (cons (make-tree this-entry
                                   left-tree
                                   right-tree)
                        remaining-elts))))))))

;;
;; (a)
;;
(list->tree '(1 3 5 7 9 11))

;;
;; the above function picks a central pivot element, constructs two
;; subtrees with elements to the left and to the right of it and
;; makes all three (the pivot element, the left and the right subtree)
;; into a new tree.

;;
;; (b)
;;
;; the number of recursive calls is logarithmic in the length of
;; input list since each call splits the input list in two parts
;; 
;; the above answer is wrong, of course, since we need to touch each
;; element of the list at least once, hence O(n) complexity
;;


;;
;; exercise 2.65
;;

;;
;; we have already have O(n) code for ordered lists, use it!
;;

(define (union-tree-set set1 set2)
  (list->tree (union-set (tree->list-2 set1) (tree->list-2 set2))))

(define (intersection-tree-set set1 set2)
  (list->tree (intersection-set (tree->list-2 set1) (tree->list-2 set2))))

;;
;; exercise 2.66
;;

(define (lookup tree key-)
  (cond ((null? tree) false)
        ((= key- (key (entry tree))) (entry tree))
        ((< key- (key (entry tree))) (lookup ( left-branch tree) key))
        ((> key- (key (entry tree))) (lookup (right-branch tree) key))))


;;
;; Example: Huffman Enconding Trees
;;

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x)
  (cadr x))

(define (weight-leaf x)
  (caddr x))

(define (make-code-tree left right)
  (list left right (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define ( left-branch tree) (car  tree))
(define (right-branch tree) (cadr tree))

;; this one is polymorphic
(define (symbols tree)
  (if (leaf? tree)
    (list (symbol-leaf tree))
    (caddr tree)))

;; this one is polymorphic, too
(define (weight tree)
  (if (leaf? tree)
    (weight-leaf tree)
    (cadddr tree)))


(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
      '()
      (let ((next-branch (choose-branch (car bits) current-branch)))
       (if (leaf? next-branch)
         (cons (symbol-leaf next-branch)
               (decode-1 (cdr bits) tree))
         (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch  branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set) (adjoin-set x (cdr set))))))

;;
;; this creates a set of pairs (sym, freq) ordered by freq
;;
(define (make-leaf-set pairs)
  (if (null? pairs)
    '()
    (let ((pair (car pairs)))
     (adjoin-set (make-leaf (car  pair)    ; symbol
                            (cadr pair))   ; frequency
                 (make-leaf-set (cdr pairs))))))

(make-leaf-set '((c 3) (b 4) (a 1) (d 3)))

;;
;; exericse 2.67
;;

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree
                      (make-leaf 'D 1)
                      (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)


;;
;; exercise 2.68
;;

(define (encode message tree)
  (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
            (encode (cdr message) tree))))

(define (encode-symbol sym tree)
  (cond ((null? tree)
         (error "error: empty tree" sym))
        ((leaf? tree) 
         (if (memq sym (symbols tree))
         '()
         (error "symbol mismatch in a leaf" sym tree)))
        (else
          (let ((lbranch ( left-branch tree))
                (rbranch (right-branch tree)))
            (cond ((memq sym (symbols lbranch))
                   (cons 0 (encode-symbol sym lbranch)))
                  ((memq sym (symbols rbranch))
                   (cons 1 (encode-symbol sym rbranch)))
                  (else
                    (error "symbol mismatch in a node" sym tree)))))))
            
(encode-symbol 'a sample-tree)

(encode-symbol 'b sample-tree)

(encode-symbol 'c sample-tree)

(encode-symbol 'd sample-tree)

(encode '(a d a b b c a) sample-tree)

(equal? sample-message (encode '(a d a b b c a) sample-tree))


;;
;; exercise 2.69
;;

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge pairs)
  (cond ((null? pairs) '())
        ((null? (cdr pairs)) (car pairs))
        (else
          (let* ((fst ( car pairs))
                 (snd (cadr pairs))
                 (tree (make-code-tree fst snd)))
            (successive-merge (adjoin-set tree (cdr (cdr pairs))))))))

            ;; (make-code-tree tree (successive-merge (cdr (cdr pairs))))))))


(generate-huffman-tree '((a 1)))

(generate-huffman-tree '((a 1) (b 2)))

(generate-huffman-tree '((a 1) (b 2) (c 3)))

(generate-huffman-tree '((c 4) (a 1) (b 2)))

(generate-huffman-tree '((c 4) (a 1) (b 2) (d 5)))

;;
;; exercise 2.70
;;

(define rock-pairs '((a    2)
                     (boom 1)
                     (get  2)
                     (job  2)
                     (sha  3)
                     (na   16)
                     (wah  1)
                     (yip  9)))

(define rock-tree (generate-huffman-tree rock-pairs))

(define rock-song
  '(get a job
        sha na na na na na na na na 
        get a job
        sha na na na na na na na na 
        wah yip yip yip yip yip yip yip yip yip
        sha boom))

(encode rock-song rock-tree)

;;
;; exericse 2.71
;;

;;
;; it's going to be a very skewed tree with each node having a single
;; leaf as one subtree, with an extremely low branching factor.
;;
;; one bit is needed for the most frequent, n bits for the least
;; frequent symbol
;;


;;
;; exercise 2.72
;;

;;
;; in a balanced tree, the depth is a log function of the number of
;; symbols
;; at each step we have to search for the symbol in both branches,
;; which decreases log-ly too
