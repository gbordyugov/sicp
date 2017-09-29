(define make-hash-table make-equal-hash-table)

(define *the-hash-table* (make-hash-table))

(define (put op type datum)
  (hash-table/put! *the-hash-table* (cons op type) datum))

(define (get op type)
  (hash-table/get *the-hash-table* (cons op type) #f))
