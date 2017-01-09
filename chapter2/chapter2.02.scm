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
        
(cc 100 us-coins)

(cc 100 uk-coins)


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
