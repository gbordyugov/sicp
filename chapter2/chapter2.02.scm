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
