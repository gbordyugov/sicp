(load "query-interpreter.scm")

(pattern-match '((? x) a (? y)) '(z a z) '())

(pattern-match (query-syntax-process '(?x a ?y)) '(z a z) '())

(unify-match (query-syntax-process '(?x a ?y))
             (query-syntax-process '(xx a ?y))
             '())

;;
;; Mickey and Minnie

(assert!
  (married Minnie Mickey))

(assert!
  (rule (married ?x ?y)
        (married ?y ?x)))

(married Mickey ?who)
