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
