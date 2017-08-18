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
