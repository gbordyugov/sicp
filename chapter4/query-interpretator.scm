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
;; accepts an expression, a frame with bindings, and a function to
;; call when a binding cannot be found in the frame
;;
;; the expression is copied and the variables are replaced by their
;; values
;;
(define (instantiate exp frame unbound-var-handler)
  (define (copy exp)
    (cond ((var? exp)
           (let ((binding (binding-in-frame exp frame)))
             (if binding
               ;; binding-value seems to be an accessor of binding
               ;; note the recursive call, since bindings can have the
               ;; form of ?x -> ?y -> 5
               (copy (binding-value binding))
               (unbound-var-handler exp frame))))
          ((pair? exp) (cons (copy (car exp)) (copy (cdr exp))))
          (else exp)))
  (copy exp))

;;
;; bindings with their constructors and accessors are defined in
;; Section 4.4.4.8
;;

;;
;; query evaluator
;;

(define (qeval query frame-stream)
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      (qproc (contents query) frame-stream)
      (simple-query query frame-stream))))

;;
;; query type and contents are defined in Section 4.4.4.7
;;


;;
;; simple queries
;;

(define (simple-query query-pattern frame-stream)
  ;; note the use of flatmap
  ;; the lambda produces a stream of frames
  ;; and the stream of those streams is flattened into just one stream
  (stream-flatmap
    (lambda (frame)
      (stream-append-delayed                          ;; Section 4.4.4.6
        (find-assertions query-pattern frame)         ;; Section 4.4.4.3
        (delay (apply-rules query-pattern frame))))   ;; Section 4.4.4.4
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

(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)
;; (put 'always-true 'qeval always-true)

;;
;; continue on 4.4.4.3 Finding Assertions by Pattern Matching
;;


;;
;; 4.4.4.4 Rules and Unification
;;

(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))

(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
        the-empty-stream
        (qeval (rule-body clean-rule)
               (singleton-stream unify-result))))))

(define (rename-variables-in rule)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp) (make-new-variable exp rule-application-id))
            ((pair? exp) (cons (tree-walk (car exp))
                               (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))

(define (unify-match p1 p2 frame)
  (cond ((eq? frame 'failed) 'failed)
        ((equal? p1 p2) frame)
        ((var? p1) (extend-if-possible p1 p2 frame))
        ((var? p2) (extend-if-possible p2 p1 frame))
        ((and (pair? p1) (pair? p2)) (unify-match (cdr p1)
                                                  (cdr p2)
                                                  (unify-match (car p1)
                                                               (car p2)
                                                               frame)))
        (else 'failed)))


;;
;; 4.4.4.5 Maintaining the Data Base
;;

(define THE-ASSERTIONS the-empty-stream)

(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-asertions)))

(define (get-all-assertions) THE-ASSERTIONS)

(define (get-indexed-assertions pattern)
  ;; index-key-of returns the index key of a pattern (is an
  ;; abstraction here)
  (get-stream (index-key-of pattern) 'assertion-stream))

(define (get-stream key1 key2)
  (let ((s (get key1 key2)))
    (if s s the-empty-stream)))

(define THE-RULES the-empty-stream)

(define (fetch-rules pattern frame)
  (if (use-index? pattern)
    (get-indexed-rules pattern)
    (get-all-rules)))

(define (get-all-rules) THE-RULES)

(define (get-indexed-rules pattern)
  (stream-append
    (get-stream (index-key-of pattern) 'rule-stream)
    (get-stream '? 'rule-stream)))

(define  (add-rule-or-assertion! assertion)
  (if (rule? assertion)
    (add-rule!       assertion)
    (add-assertiont! assertion)))

(define (add-assertion! assertion)
  (store-assertion-in-index assertion)
  (let ((old-assertion THE-ASSERTIONS))
    (set! THE-ASSERTIONS (cons-stream assertion old-assertion))
    'ok))

(define (add-rule! rule)
  (store-rule-in-index rule)
  (let ((old-rules THE-RULES))
    (set! THE-RULES (cons-stream rule old-rules))
    'ok))

(define (store-assertion-in-index assertion)
  (if (indexable? assertion)
    (let ((key (index-key-of assertion)))
      (let ((current-assertion-stream (get-stream key 'assertion-stream)))
        (put key 'assertion-stream
             (cons-stream assertion current-assertion-stream))))))

(define (store-rule-in-index rule)
  (let ((pattern (conclusion rule)))
    (if (indexable? pattern)
      (let ((key (index-key-of pattern)))
        (let ((current-rule-stream (get-stream key 'rule-stream)))
          (put key 'rule-stream (cons-stream rule current-rule-stream)))))))

(define (indexable? pat)
  (or (constant-symbol? (car pat))
      (var? (car pat))))

(define (index-key-of pat)
  (let ((key (car pat)))
    (if (var? key) '? key)))

(define (use-index? pat)
  (constant-symbol? (car pat)))

;;
;; Exercise 4.70
;;

;;
;; without 'let', assertions would be defined as an infinite stream,
;; consisting of the same assertion, analogous to the stream of ones
;; of the hint
;;



;;
;; 4.4.4.6 Stream Operations
;;


(define (stream-append-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream (stream-car s1) (stream-append-delayed (stream-cdr s1)
                                                        delayed-s2))))

(define (interleave-delayed s1 delayed-s2)
  (if (stream-null? s1)
    (force delayed-s2)
    (cons-stream (stream-car s1)
                 (interleave-delayed (force delayed-s2)
                                     (delay (stream-cdr s1))))))

(define (stream-flatmap proc s)
  (flatten-stream (stream-map proc s)))

(define (flatten-stream stream)
  (if (stream-null? stream)
    the-empty-stream
    (interleave-delayed (stream-car stream)
                        (delay (flatten-stream (stream-cdr stream))))))

(define (singleton-stream x)
  (cons-stream x the-empty-stream))


;;
;; 4.4.4.7 Query Syntax Procedures
;;

;;
;; accessors
;;
(define (type exp)
  (if (pair? exp)
    (car exp)
    (error "Unknown expression TYPE" exp)))

(define (contentes exp)
  (if (pair? exp)
    (cdr exp)
    (error "Unknown expression CONTENTS" exp)))


;;
;; those are used by query-driver-loop, Section 4.4.4.1
;;

(define (assertion-to-be-added? exp)
  (eq? (type exp) 'assert!))

;; just the body of assertions
(define (add-assertion-body exp)
  (car (contents exp)))

(define (empty-conjunction? exps) (null? exps))
(define (first-conjunct     exps) (car   exps))
(define ( rest-conjuncts    exps) (cdr   exps))

(define (empty-disjunction? exps) (null? exps))
(define (first-disjunct     exps) (car   exps))
(define ( rest-disjuncts    exps) (car   exps))

(define (negated-query exps) (car exps))
(define (predicate exps) (car exps))
(define (args exps) (cdr exps))

(define (rule? statement)
  (tagged-list? statement 'rule))

(define (conclusion rule)
  (cadr rule))

(define (rule-body rule)
  (if (null? (cddr rule))
    '(always-true)
    (caddr rule)))

;;
;; procedures to map ?x => (? x)
;;

(define (query-syntax-process exp)
  (map-over-symbols expand-question-mark exp))

(define (map-over-symbols proc exp)
  (cond ((pair? exp) (cons (map-over-symbols proc (car exp))
                           (map-over-symbols proc (cdr exp))))
        ((symbol? exp) (proc exp))
        (else exp)))

(define (expand-question-mark symbol)
  (let ((chars (symbol->string symbol)))
    (if (string=? (substring chars 0 1) "?")
      (list '? (string->symbol (substring chars 1 (string-length chars))))
      symbol)))


(define (var? exp) ;; (? x) coming from expanding ?x
  (tagged-list? exp '?))

(define (constant-symbol? exp) ;; a 'normal' symbol
  (symbol? exp))


;;
;; infrastructure for unique variables, which are constructed during
;; application of rules
;;

(define rule-counter 0)

(define (new-rule-application-id)
  (set! rule-counter (+ 1 rule-counter))
  rule-counter)

(define (make-new-variable var rule-application-id)
  (cons '? (cons rule-application-id (cdr var))))


;;
;; getting rid of question marks
;;

(define (contract-question-mark variable)
  (string->symbol
    (string-append "?"
                   (if (number? (cadr variable))
                     (string-append (symbol->string (caddr variable))
                                    "-"
                                    (number->string (cadr variable)))
                     (symbol->string (cadr variable))))))



;;
;; 4.4.4.8 Frames and Bindings
;;

(define (make-binding variable value)
  (cons variable value))

(define (binding-variable binding) (car binding))
(define (binding-value    binding) (cdr binding))

(define (binding-in-frame variable frame)
  (assoc variable frame))

(define (extend variable value frame)
  (cons (make-binding variable value) frame))
