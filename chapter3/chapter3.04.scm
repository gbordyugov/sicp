;;
;; exercise 3.38
;;
;;
;; basically, there are 6 permutations of 3 elemenents, I trust I can
;; do the arythmetics
;;

;;
;; exercise 3.39
;;
;; it's three possibilities, depending whether the second procedure
;; runs a) before the first serialized lambda b) between it and
;; assignment to x in the first procedure and c) after both
;;


;;
;; exercise 3.40
;;
;; both serialized funcs commute and the computation would produce the
;; same result independently of the order the serialized funcs are
;; called in.
;;

;;
;; exercise 3.41
;;
;; both deposit and withdraw read balance only once, hence I see no
;; need to protect the balance query
;;

;;
;; exercise 3.42
;;
;;
;; in the original version, we create the protected procedures on the
;; fly in each call of dispatch. Here, we pre-cache them. Seems OK to
;; me
;;
