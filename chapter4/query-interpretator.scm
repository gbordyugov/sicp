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
