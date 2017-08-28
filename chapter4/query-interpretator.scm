;;
;; 4.4.2 How the Query System Works
;;

;;
;; two possibilities: either amb-similar design or stream of frames
;;

;;
;; Two ideas:
;;
;; 1. Pattern matching
;;
;; 2. Unification
;;


;;
;; Pattern matching
;;
;; ((a b) c (a b)) matches (?x c ?x) with ?x bound to (a b)
;;
;; Input of the pattern matcher:
;;  - 1. a pattern
;;  - 2. a piece of data
;;  - 3. a frame possibly specifying bindings (from previous pattern
;;  matching possibly
;;
;; (?x ?y ?x) will match (a b a) given an empty frame
;;
;; (?x ?y ?x) will fail to match (a b a) given a frame with ?b bound to c
;;
;; Pattern matching is the only mechanism needed to process simple
;; queries
;;


;;
;; Streams of frames
;;

;;
;; given a single frame, the matching process runs through the DB
;; entries one by one. For each entry, the matcher generates either a
;; mismach or an extension to the frame
;;
;; the results (frames? frame extensions?) are collected into a stream
;; which goes through a filter to drop mismatches
;;

;;
;; a query takes an input stream of frames
;; for each frame, it runs the matcher against DB result in possibly
;; new frames
;; for each frame from the input stream of frames, the output frames
;; are combined into one big stream
;;


;;
;; Compound queries
;;

;;
;; compound queries are formed by AND, OR, and NOT operations.
;;

;;
;; AND queries
;;

;;
;; (and Q1 Q2) first produces a stream of frames for query Q1 and than
;; uses this stream as an input for query Q2
;;

;;
;; OR queries
;;

;;
;; (or Q1 Q2) produces in parallel a stream of frames for query Q1 and
;; a stream of frames for query Q2 and then merges both streams
;;

;;
;; NOT queries
;;

;; (not Q1) removes from the input stream all frames for which Q1 is
;; satisfied
;;

;;
;; lisp-value works in a similar way
;;



;;
;; Unification
;;

;;
;; unification is a generalization of pattern matching to the extent
;; that both the piece of data and the pattern may contain variables
;;

;;
;; for example, unifying (?x a ?y) and (?y ?z a) will result in a
;; frame where all ?x, ?y, and ?z are bound to `a`.
;;
;; On the other hand, unifying (?x ?y a) and (?x b ?y) would fail,
;; since there is no way to satisfy ?y = b and a = ?y simultaneously
;;

;;
;; the unifier is an equation-solver
;;

;;
;; for example, unifying (?x ?x) and ((a ?y c) (a b ?z)) must result
;; in ?z = c, ?y = b anx ?x = (a b c)
;;

;;
;; consider the unification of (?x a) and ((b ?y) ?z)
;;
;; we can deduce that
;; ?x = (b ?y)
;; a  = ?z
;;
;; whereas ?z has been found, ?x and ?y are open up to the functional
;; dependency between them. The unifier would bin ?x to the expression
;; (b ?y) in the resulting frame. If ?y is determined later, the
;; corresponding ?x would refer to the corresponding value of (b ?y)
;;


;;
;; Applying rules
;;

;;
;; evaluating
;;
;; (lives-near ?x (Hacker Alyssa P))                               (*)
;;
;; results in trying to unify the above pattern with the conclusion of
;; each rule, matching the rule
;;
;; (rule (lives-near ?person-1 ?person-2)
;;       (and (address ?person-1 (?town . ?rest-1))
;;            (address ?person-2 (?town . ?rest-2))
;;            (not (same ?person-1 ?person-2))))
;;
;; unifying creates/extends a frame by binding ?person-2 to (Hacker
;; Alyssa P)
;;
;; relative to this new frame, the body of the rule is evaluated.
;; Successful matches will extend this frame by providing a binding
;; for ?person-1, and consequently a value for ?x which enters the
;; original query patter (*)
;;

;;
;; The method fo applying a rule
;;
;; 1. Unify the query with the conclusion of the rule to form an
;; extension of the original frame
;;
;; 2. Relative to the extended frame, evaluate the query formed by the
;; body of the rule
;;

;;
;; is quite similar to the rules for applying a procedure in the
;; `normal` Lisp
;;

;;
;; procedure definitions are the means of abstractions in Lisp
;; rules definitions are the means of abstractions in the query
;; language
;;
;; the abstraction is unwinded by creating appropriate bindings and
;; evaluating the rule or procedure body relative to these
;;


;;
;; Simple queries
;;

;;
;; before: simple queries in absence of rules
;;
;; now that we know how to evaluate rules, we can describe how to
;; evaluate simple queries by using both rules and assertions
;;

;;
;; Given the query pattern and a stream of frames, we produce, for
;; each frame in the input stream, two streams:
;;
;; 1. A stream of extended frames obtained by matching the patter
;; against all assertions in the DB (using the pattern matcher), and
;;
;; 2. A stream of extened frames obtained by applying all possible
;; rules (using the unifier).
;;
;; Appending both streams produces a stream that consist of all the
;; ways that the given pattern can be satisfied consistent with the
;; original frame.
;;


;;
;; The query evaluator and the driver loop
;;

;;
;; (assert! ...)
;;
;; adds an assertion or rule to the database
;;

;;
;; why streams and not lists: the output can be possibly infinitely
;; long
;;


;;
;; 4.4.3 Is Logic Programming Mathematical Logic?
;;

;;
;; (spoiler: not really)
;;

;;
;; Infinite loops
;;
;; (assert! (married Minnie Mickey))
;; (assert! (rule (married ?x ?y) (married ?y ?x)))
;;
;; then (married Mickey ?who) will produce an infinite loop
;;

;;
;; Problems with `not`
;;

;;
;; The following two queries are not equivalent:
;;
;; (and (supervisor ?x ?y)
;;      (not (job ?x (computer programmer))))
;;
;; and
;;
;; (and (not (job ?x (computer programmer)))
;;      (supervisor ?x ?y))
;;
;;
;; Remark 78 (about (not (baseball-fan (Bitdiddle Ben)))) is awesome
;;


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
