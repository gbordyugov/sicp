;;
;; With ordinary lists, both the `car` and the `cdr` are evaluated at
;; construction time. With streams, the `cdr` is evaluated at
;; selection time.
;;


;;
;; the two stream axioms:
;;
;; (stream-car (cons-stream x y)) = x
;; (stream-cdr (cons-stream x y)) = y
;;
;;
;; (cons-stream <a> <b>)
;; is equivalent to
;; (cons <a> (delay <b>))
;;
;; (define (stream-car s) (car s))
;; (define (stream-cdr s) (force (cdr stream)))

(define (memo-proc proc)
  (let ((already-run? false)
        (result       false))
    (lambda ()
      (if (not already-run?)
        (begin
          (set! result (proc))
          (set! already-run true)
          result)
        result))))

;; so (delay <exp>) is equiavalent to
;; (memo-proc (labda () <exp>))
