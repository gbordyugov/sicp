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
