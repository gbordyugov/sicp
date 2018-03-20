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
;; OK, what I start understanding now: the pointer to the list ((1 2)
;; 3 4) is stored at index 1
;;
;; it's car is p5, i.e. pointer to a pair stored at location 5
;; at location 5, we've got again a car, given by n1 (just a number)
;; and a cdr given by p7, which in turn points at car=n2 and cdr=e0
;;
;; the cdr of the initial par points to p2, which in turn is a number
;; 3 plus cdr pointing to p4, which is a car=n4 and cdr=e0
;;

;;
;; A symbol is represented as a typed pointer that designates a
;; sequence of the characters that form the printed representation of
;; the symbol
;;
;; In order to ensure that every symbol appears in the system only
;; once, there is the `obarray' table that keeps the set of all
;; symbols that have been seen so far. When a symbol is encountered by
;; the reader, it's checked against obarray - if it's present, the
;; reader get the address of the existing symbol, otherwise it creates
;; a new one.
;;
;; The process of replacing character strings by unique pointers is
;; called `interning' symbols
;;

