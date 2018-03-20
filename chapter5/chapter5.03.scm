;;
;; 5.3 Storage Allocation and Garbage Collection
;;

;;
;; idea: equip the machine with list-structured memory, with
;; primitives being the usual list operations (authors understand
;; though that there must be another layer of abstraction between this
;; list-structured memory and real memory / hardware)
;;

;;
;; Considerations:
;;
;; 1. How to represent the "box-and-pointer" structure of Lisp pairs
;; using the normal memory
;; 2. Memory management: creating and destroying conses
;;

;;
;; 5.3.1 Memory as Vectors
;;

;;
;; interface of memory:
;;
;; (vector-ref <vector> <n>) - returns the n-th element
;; (vector-put! <vector> <n> <value>) sets the n-th element to value
;;

;;
;; Representin Lisp data
;;

;;
;; the-cars and the-cdrs vectors
;;
;; Quotation: We will represent list structure as follows: A pointer
;; to a pair is an index into the two vectors.
;;
;; Q: `an index into two vectors` - is it a pair of indices?
;;

;;
;; typed pointers include information on data type
;;
;; there can be pointers to pairs, and atoms
;;

;;
;; two data objects are considered to be the same (in the sense of
;; eq?) if their pointers are identical.
;;

;;
;; I don't really understand Figure 5.14 right now
;;
