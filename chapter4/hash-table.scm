;;
;; just a shortcut for the built-in
;;

(define make-hash-table make-equal-hash-table)


;;
;; those are used to index operations on queries
;;

(define *the-hash-table* (make-hash-table))

(define (put op type datum)
  (hash-table/put! *the-hash-table* (cons op type) datum))

(define (get op type)
  (hash-table/get *the-hash-table* (cons op type) #f))


;;
;; those are needed for loop detection
;;

(define *history* (make-hash-table))

(define (history-put key)
  (hash-table/put! *history* key #t))

(define (history-get key)
  (hash-table/get *history* key #f))
