(define (add-rat x y)
  (make-rate (+ (* (number x) (denom y))
                (* (number y) (denom x)))
             (* (denom x) (denom y))))
