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

;;
;; exercise 3.50
;;

(define (stream-map proc . argstreams)
  (if (null? (car argstreams))
    the-empty-stream
    (stream-cons
      (apply proc       (map stream-car argstreams))
      (apply stream-map (cons proc (map (stream-cdr) argstreams))))))


;;
;; exercise 3.51
;;

;;
;; will show numbers up to 5 first, then those between 5 and 7
;;


;;
;; exercise 3.52
;;
;; the point here is that sum is not accumulated fully over the whole
;; stream, but rather over the portion of it that has been used
;; (either by stream-filter or something else)
;;
;; memoizing would prevent adding things to sum twice
;;

;;
;; 3.5.2 Infinite streams
;;

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))

(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))
