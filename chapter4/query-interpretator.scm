;;
;; 4.4.4 Implementing the Query System
;;


;;
;; The Driver Loop and Insatantiation
;;

(define  input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  (let ((q (query-syntax-process (read))))
    (cond ((assertion-to-be-added? q)
           (add-rule-or-assertion! (add-assertion-body q))
           (newline)
           (display "Assertion added to data base.")
           (query-driver-loop))
          (else
            (newline)
            (display output-prompt)
            (display-stream
              (stream-map
                (lambda (frame)
                  (instantiate ;; this instantiates query by variable bindings from frames
                    q frame
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))

;;
;; accepts an expression, a frame with bindings plus a function to
;; call when a binding cannot be found in the frame
;;
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
               (copy (binding-value binding))
               (unbound-var-handler exp frame))))
          ((pair? exp)
           (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))


(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      (qproc (contents query) frame-stream)
      (simple-query query frame-stream))))


;;
;; simple queries
;;

(define (simple-query query-pattern frame-stream)
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed
        (find-assertions query-pattern frame)
        (delay (apply-rules query-pattern frame))))
    frame-stream))


;;
;; compound queries
;;

;;
;; conjoin takes care of `and` queries
;;
;; conjoin takes as inputs the conjucts and the frame stream and the
;; stream of extended frames
;;

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts) frame-stream))))
;; (put 'and 'qeval conjoin)


;;
;; disjoin takes care of `or` queries
;;

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))
;; (put 'or 'qeval disjoin


;;
;; filters take care of `not` queries
;;

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (stream-null?
            (qeval (negated-query operands) (singleton-stream frame)))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream))
;; (put 'not 'qeval negate)


(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var: LISP-VALUE" v))))
        (singletone-stream frame)
        the-empty-stream))
    frame-stream))
;; (put 'lisp-value 'qeval lisp-value)
