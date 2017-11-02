;;
;; 4.4.4 Implementing the Query System
;;

(load "hash-table.scm")
;;
;; a little helper function
;;
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    false))



;;
;; 4.4.4.1 The Driver Loop and Insatantiation
;;

(define  input-prompt ";;; Query input:")
(define output-prompt ";;; Query results:")

(define (query-driver-loop)
  (prompt-for-input input-prompt)
  ;; map variables to pairs, i.e. ?x => (? x)
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
                    ;; this is the unbound-var-handler below
                    (lambda (v f)
                      (contract-question-mark v))))
                (qeval q (singleton-stream '()))))
            (query-driver-loop)))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

;;
;; accepts an expression, a frame with bindings, and a function to
;; call when a binding cannot be found in the frame
;;
;; the expression is copied and the variables are replaced by their
;; values found in the data frame, the unbound variables are handed
;; over to unbound-var-handler, the result of the call being put in
;; their (unbound variables') place
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
;; 4.4.4.2 The Evaluator
;;

(define (qeval query frame-stream)
  ;; look up in the database of evaluators whether there is an
  ;; evaluator for this type of query
  (let ((qproc (get (type query) 'qeval)))
    (if qproc
      ;; if there is, call it
      (qproc (contents query) frame-stream)
      ;; otherwise assume it's a 'simple' query and call the simple
      ;; query evaluator (next function)
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
        ;; TODO: why using delay here?
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
;; here, conjuncts are given by contents (cdr) of the query (i.e.
;; everything but the `and` keyword
;;

(define (conjoin conjuncts frame-stream)
  (if (empty-conjunction? conjuncts)
    frame-stream
    (conjoin (rest-conjuncts conjuncts)
             (qeval (first-conjunct conjuncts) frame-stream))))

(put 'and 'qeval conjoin)


;;
;; disjoin takes care of `or` queries
;;
;; here, disjuncts are given by the contents (cdr) of the query (i.e.
;; everything but the `or` keyword
;;

(define (disjoin disjuncts frame-stream)
  (if (empty-disjunction? disjuncts)
    the-empty-stream
    (interleave-delayed
      (qeval (first-disjunct disjuncts) frame-stream)
      (delay (disjoin (rest-disjuncts disjuncts) frame-stream)))))

(put 'or 'qeval disjoin)


;;
;; filters take care of `not` queries
;;
;; here, operands are given by the contents (cdr) of the query (i.e.
;; everything but the `not` keyword
;;

(define (negate operands frame-stream)
  (stream-flatmap
    (lambda (frame)
      ;; try to eval query on that frame
      (if (stream-null?
            (qeval (negated-query operands) (singleton-stream frame)))
        ;; it the result is null, return the frame
        (singleton-stream frame)
        ;; otherwise return the empty stream
        the-empty-stream))
    frame-stream))

(put 'not 'qeval negate)


;;
;; call is the contents (cdr) of the query, i.e. what follows the
;; `lisp-value` keyword
;;
;; example: (and (salary ?person ?amount)
;;               (lisp-value > ?amount 30000))
;;
(define (lisp-value call frame-stream)
  (stream-flatmap
    (lambda (frame)
      (if (execute
            (instantiate
              call
              frame
              (lambda (v f)
                (error "Unknown pat var: LISP-VALUE" v))))
        (singleton-stream frame)
        the-empty-stream))
    frame-stream))

(put 'lisp-value 'qeval lisp-value)

;;
;; eval is used by lisp-value
;;
(define (execute exp)
  (apply (eval (predicate exp) user-initial-environment)
         (args exp)))

(define (always-true ignore frame-stream) frame-stream)

(put 'always-true 'qeval always-true)

;;
;; 4.4.4.3 Finding Assertions by Pattern Matching
;;

;;
;; goes through the database of assertions and tries to match the
;; pattern against them
;;
;; usees fetch-assertion to pre-filter data-base entries (assertions)
;; to minimize the number of calls of check-an-assertion/pattern-match
;;
(define (find-assertions pattern frame)
  (stream-flatmap
    (lambda (datum)
      (check-an-assertion datum pattern frame))
    ;; fetch-assertions deals with optimizing the search in the DB
    ;; using indexing
    (fetch-assertions pattern frame)))


;;
;; try to match the query pattern against just one assertion
;;
;; returns either an empty stream (if there is no match)
;; or a stream consisting just of one frame (match-result)
;;
(define (check-an-assertion assertion query-pat query-frame)
  (let ((match-result (pattern-match query-pat assertion query-frame)))
    (if (eq? match-result 'failed)
      the-empty-stream
      (singleton-stream match-result))))


;;
;; the core pattern matching procedure
;;
;; pat - the pattern, like (job ?who ?what)
;; dat - the datum,   like (job me   schemer)
;; frame - contains previous bindings, like ?x ->5, ?name -> Grisha
(define (pattern-match pat dat frame)
  (cond
    ;; fail fast
    ((eq? frame 'failed) 'failed)
    ;; no new bindings, just return the input frame
    ((equal? pat dat) frame)
    ;; if pattern is a variable, check if the variable already has
    ;; a binding the frame and the new and the old bindings are
    ;; not in conflict
    ((var? pat) (extend-if-consistent pat dat frame))
    ;; the actual tree recursion
    ;; note that the resutl of pattern-match of cars is used as
    ;; an input frame for pattern matching of cdrs
    ((and (pair? pat) (pair? dat))
     (pattern-match (cdr pat) (cdr dat)
                    (pattern-match (car pat) (car dat) frame)))
    (else 'failed)))


;;
;; check if var is already bound in frame
;; if not, extend the frame and return it
;; if yes and the binding is     the same, return the frame
;; if yes and the binding is not the same, fail
(define (extend-if-consistent var dat frame)
  (let ((binding (binding-in-frame var frame)))
    (if binding
      ;; TODO: why additional pattern-match here?
      ;; answer, as in text: without rules, it's only values to
      ;; compare however, with rules the stored variable can contain
      ;; pattern variables coming from the right-hand sides of
      ;; unifications and they have to be pattern-matched against data
      (pattern-match (binding-value binding) dat frame)
      (extend var dat frame))))



;;
;; 4.4.4.4 Rules and Unification
;;

;;
;; applies all rules from the data base, similar to find-assertions
;; above
;;
(define (apply-rules pattern frame)
  (stream-flatmap (lambda (rule)
                    (apply-a-rule rule pattern frame))
                  (fetch-rules pattern frame)))


;;
;; applies a single rule
;; tries to resolve possible name collisions by renaming variables in
;; the rule by appending a number to them
;; otherwise it can happen that two rules use the same variable name
;; ?x and extending a frame using those rules would result in a
;; collision
;;
(define (apply-a-rule rule query-pattern query-frame)
  (let ((clean-rule (rename-variables-in rule)))
    ;; unify query with the conclusion of the rule
    (let ((unify-result (unify-match query-pattern
                                     (conclusion clean-rule)
                                     query-frame)))
      (if (eq? unify-result 'failed)
        ;; if unification fails, just return the empty stream
        the-empty-stream
        ;; othewise queval the body of the rule with respect to the
        ;; frame produced by unification
        (qeval (rule-body clean-rule)
               (singleton-stream unify-result))))))


(define (rename-variables-in rule)
  ;; each rule application gets its unique id, i.e. all its variables
  ;; would have the same suffix (like x-113)
  (let ((rule-application-id (new-rule-application-id)))
    (define (tree-walk exp)
      (cond ((var? exp) (make-new-variable exp rule-application-id))
            ((pair? exp) (cons (tree-walk (car exp))
                               (tree-walk (cdr exp))))
            (else exp)))
    (tree-walk rule)))


;;
;; the logic is not really different from the pattern matcher
;; except that we expect variables on the right-hand side (i.e. p2) of
;; the equation
;;
(define (unify-match p1 p2 frame)
  (cond
    ;; fail fast
    ((eq? frame 'failed) 'failed)
    ;; no new information, just return the frame
    ((equal? p1 p2) frame)
    ;; ok, p1 is a variable (like (? x)), try to extend the frame
    ((var? p1) (extend-if-possible p1 p2 frame))
    ;; that's the main difference from the pattern matcher
    ;; p1 is not a variable (otherwise it's caught by the last case)
    ;; p2 is a variable
    ;; note the reversed order of p1 and p2 in the call of
    ;; extend-if-possible
    ((var? p2) (extend-if-possible p2 p1 frame))
    ;; recurse on both trees, using unify-match on cars as the input
    ;; frame to unify-match on cdrs
    ((and (pair? p1) (pair? p2))
     (unify-match (cdr p1) (cdr p2)
                  (unify-match (car p1) (car p2) frame)))
    (else 'failed)))


(define (extend-if-possible var val frame)
  ;; try to find an existing binding for var
  (let ((binding (binding-in-frame var frame)))
    (cond
      ;; if there is a binding, unify-match its rhs with val
      (binding
        (unify-match (binding-value binding) val frame))
      ;; now value happens to be a variable, too
      ((var? val)
       (let ((binding (binding-in-frame val frame)))
         (if binding
           ;; if this variable is already bound, unify var with the
           ;; rhs of the binding, note the recursion
           (unify-match var (binding-value binding) frame)
           ;; otherwise just extend the original frame by the binding
           (extend var val frame))))
      ;; in general, we give up unifying ?x with arbitrary f(?x)
      ((depends-on? val var frame)
       'failed)
      ;; the easiest case, just extend the frame
      (else
        (extend var val frame)))))

(define (depends-on? exp var frame)
  (define (tree-walk e)
    (cond ((var? e) (if (equal? var e)
                      true
                      (let ((b (binding-in-frame e frame)))
                        (if b
                          (tree-walk (binding-value b))
                          false))))
          ((pair? e) (or (tree-walk (car e))
                         (tree-walk (cdr e))))
          (else false)))
  (tree-walk exp))


;;
;; 4.4.4.5 Maintaining the Data Base
;;

(define THE-ASSERTIONS the-empty-stream)

;;
;; fetch-assertion aims at optimizing the search (by indexing, for
;; instaince
;;
(define (fetch-assertions pattern frame)
  (if (use-index? pattern)
    (get-indexed-assertions pattern)
    (get-all-assertions)))

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
    (add-rule!      assertion)
    (add-assertion! assertion)))

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

(define (display-stream s)
  (stream-for-each display-line s))

(define (display-line x) (newline) (display x))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))


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

(define (contents exp)
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
(define ( rest-disjuncts    exps) (cdr   exps))

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
