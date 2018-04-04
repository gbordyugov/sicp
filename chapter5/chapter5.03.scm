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
;; A: one need just one index, since both vectors are `parallel'
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

;;
;; Implementing the primitive list operations
;;

;; (assign <reg1> (op car) (reg <reg2>))
;; is equivalent to
;; (assign <reg1> (op vector-ref) (reg the-cars) (reg <reg2>))
;; dito for cdr
;;
;; the instruction
;; (perform (op set-car!) (reg <reg1>) (reg <reg2>))
;; is equivalent to
;; (perform (op vector-set!) (reg the-cars) (reg <reg1>) (reg <reg2>))
;; dito for cdr

;;
;; cons is performed by allocating an unused index ant storing the
;; arguments to cons in the-cars and the-cdrs at that indexed vector
;; positions.
;;
;; A special register named `free` is assumed to to hold a pair
;; pointer containing the next available index and we can increment
;; the index part of that pointer to find the next free location.
;;
;; the instruction
;; (assign <reg1> (op cons) (reg <reg2>) (reg <reg3>))
;; is implemented as
;; (perform (op vector-set!) (reg the-cars) (reg free) (reg <reg2>))
;; (perform (op vector-set!) (reg the-cdrs) (reg free) (reg <reg3>))
;; (assign <reg1> (reg free))
;; (assign free (op +) (reg free) (const 1))

;;
;; the eq? operation like in (op eq?) (reg <reg1>) (reg <reg2>) simply
;; tests the equality of all fields in the registers

;;
;; Implementing stacks
;;
;; (save <reg>)
;; can be implemented as
;; (assign the-stack (op cons) (reg <reg>) (reg the-stack))
;;
;; similarly, (restore <reg>) can be implemented as
;; (assign <reg> (op car) <reg the-stack))
;; (assign the-stack (op cdr) (reg the-stack))
;; and (perform (op initialize-stack)) can be implemented as
;; (assign the-stack (const ()))

;;
;; exercise 5.20
;;

;;
;; I drew it, promised!
;;

;;
;; exercise 5.21 (a)
;;

;;
;; just a reminder how it worked
;;
(controller
  (save continue)                    ;; from the previous context
  (assign continue (label fib-done)) ;; where to return after we're done, is somewhere else
  (goto (label fib-loop))            ;; call the function

  fib-loop                           ;; entry point to the subroutine
  (test (op <) (reg n) (const 2))
  (branch (label immediate-answer))
  ;; preparing to compute Fib(n-1)
  (save continue)
  (assign continue (label afterfib-n-1))
  (save n)
  (assign n (op -) (reg n) (const 1))
  (goto (label fib-loop))   ;; perform recursive call

  afterfib-n-1              ;; upon return, val contains Fib(n-1)
  (restore n)
  (restore continue)
  ;; set up to compute Fib(n-2)
  (assign n (op -) (reg n) (const 2))
  (save continue)
  (assign continue (label afterfib-n-2))
  (save val)                ;; save Fib(n-1)
  (goto (label fib-loop))   ;; perform recursive call

  afterfib-n-2
  (aassign n (reg val))     ;; n   <- Fib(n-2)
  (restore val)             ;; val <- Fib(n-1)
  (restore continue)
  (assign val (op +) (reg val) (reg n))
  (goto (reg continue))     ;; return to caller

  immediate-answer
  (assign val (reg n))      ;; base case Fib(n) = n
  (goto (reg continue))
  )

;;
;; the actual code
;;

(controller
  count-leaves-loop
  (test (op null?) (reg tree))
  (branch (label null-label))
  (test (op pair?) (reg tree))
  (branch (label default-case))
  (goto (label not-pair-label))

  default-case
  (save tree)
  (assign tree (op car) (reg tree))
  ;; this is a call
  (save continue)
  (assign continue (label after-count-leaves-car))
  (goto (label count-leaves-loop))

  after-count-leaves-car
  (assign n1 (reg val))
  (restore tree)
  (restore continue)
  (save tree)
  (assign tree (op cdr) (reg tree))
  ;; this is a call
  (save continue)
  (assign continue (label after-count-leaves-cdr))
  (goto (label count-leaves-loop))

  after-count-leaves-cdr
  (assign n2 (reg val))
  (restore tree)
  (restore continue)
  (assign val (op +) (reg n1) (reg n2))
  (goto (reg continue))

  null-label
  (assign val (const 0))
  (goto (reg continue))

  not-pair-label
  (assign val (const 1))
  (goto (reg continue)))

;;
;; exercise 5.21 (b)
;;

(controller
  (assign n (const 0))

  count-leaves-loop
  (test (op null?) (reg tree))
  (branch (label null-tree))

  (test (op pair?) (reg tree))
  (goto (label default-case))
  (branch (label not-pair-label))

  default-case
  (save tree)
  (assign tree (op car) (reg tree))
  (save continue)
  (assign continue (label after-first-call))
  (goto (label count-leaves-loop))

  after-first-call
  (assign n1 (reg val))
  (restore continue)
  (restore tree)
  (save tree)
  (assign tree (op cdr) (reg tree))
  (save n)
  (assign n (reg n1))
  (save continue)
  (assign continue (label after-second-call))
  (goto (label count-leaves-loop))

  after-second-call
  (restore continue)
  (restore n)
  (restore tree)
  (goto (reg continue))

  not-pair-label
  (assign val (op +) (reg n) (const 1))
  (goto (reg continue))

  null-tree
  (assign val (reg n))
  (goto (reg continue)))


;;
;; exercise 5.22 (a)
;;

(define (append x y)
  (if (null? x)
    y
    (cons (car x) (append (cdr x) y))))

(controller
  ;; input: x, y
  ;; returns val
  append-main-loop
  (test (op null?) (reg x))
  (branch (label null-label))

  recursion
  (save x)
  (assign x (op cdr) (reg x))
  (save continue)
  (assign continue (label after-recursion))
  (goto (label append-main-loop))

  after-recursion
  ;; now val holds the result of (append (cdr x) y)
  (restore continue)
  (restore x)
  (assign car-x (op car) (reg x))
  (assign val (op cons) (reg car-x) (reg val))
  (goto (reg continue)) ;; return


  null-label
  (assign val (reg y))
  (goto (reg continue)))

;;
;; exercise 5.22 (b)
;;

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(controller
  append!-entry
  ;; input x y
  ;; returns x
  ;; uses val
  (save continue)
  (assign continue (after-last-pair-call))
  (goto (label last-pair-loop))

  after-last-pair-call
  (restore continue)
  (assign x (op set-cdr!) (reg val) (reg y))
  (goto (reg continue)) ;; return

  last-pair-loop
  ;; input x
  ;; returns val
  ;; uses cdr-x
  (assign cdr-x (op cdr) (reg x))
  (test (op null?) (reg cdr-x))
  (branch (label null-cdr-x))

  (assign x (reg cdr-x))
  ;; tail recursion -- look ma, no stack!
  (goto (label last-pair-loop))

  null-cdr-x
  (assign val (reg x))
  (goto (reg continue)))

;;
;; 5.3.2 Maintaining the Illusion of Infinite Memory
;;

;;
;; memory is taken up by ``consing'' -- building new pairs
;;

;;
;; Observation: at any moment in a Lisp interpretation, the only
;; objects that can affect the future of the computation are those
;; that can be reached by some succession of car and cdr operations
;; starting from the pointers that are currently in the machine
;; registers
;;

;; The ``stop and copy'' strategy: We're having a meeting now..., to
;; be continued
;;
;; ``working memory'' + ``free memory''
;; all new conses are allocated in working memory. Once working memory
;; is full, all used conses are carried over to free memory, which
;; turns into working memory and the old working memory becomes free
;; memory.

;;
;; Implementation of a stop-and-copy garbage collector
;;

;;
;; a register called ``root'' that hold a pointer to a structure that
;; eventually points at all accessible data. This can be arranged by
;; storing the contents of all the machine registers in a
;; pre-allocated list pointed at by ``root'' just before starting
;; garbage collection (all registers except the GC/memory-relevant
;; ones like ``root'', the-cars, the-cdrs, etc.).
;;
;; Working memory with the-cars and the-cdrs and free memory with
;; free-cars and free-cdrs.
;;
;; GC is triggered when a cons operation attempts to increment the
;; free pointer beyond the end of the memory vector.
;;
;; When a GC run is done, root point into the new memory and all
;; objects accessible from the root will have been moved into the new
;; memory, and the free pointer will indicate the next place in the
;; new memory where a new pair can be allocated. The roles of new and
;; working memory will have been swapped.
;;

;;
;; The state of the GC process is controlled by maintaining two
;; pointers: free and scan. These are intialized to pint to the
;; beginning of the new memory. First, we relocate the pair pointed at
;; by root to the beginning of the new memory. The pair is copied, the
;; root pointer is adjusted to point to the new location and the free
;; pointer is incremented. Additionally, the old location of the pair
;; is marked to show that its contents have been moved. The marking is
;; done as follows: In the car position, we place a special tag that
;; signals that this is an already-moved object. (Such an object is
;; traditionally called a ``broken heart''). In the cdr position a
;; forwarding address is placed that points at the location to which
;; the object has been moved.
;;
;; The main loop begins: At each step, the scan pointer (initially
;; pointing at the relocated root) points at a pair that has been
;; moved to the new memory but whose car and cdr pointers still refer
;; to objects in the old memory. These objects are each relocated
;; (recursively, I guess -gb-), and the scan pointer is incremented.
;;
;; To relocate an object (for example, the object indicated by the car
;; pointer of the pair we're scanning), we check to see if the object
;; has already been moved, as indicated by the presence of a
;; broken-heart tag in the car position of the object (the object can
;; have been already moved if it was part of a previously considered
;; object). If the object has not already been moved, we copy it to
;; the place indicated by free, update free, set up a broken heart at
;; the object's old location, and update the pointer to the object (in
;; this example, the car pointer of the pair we are scanning) to point
;; to the new location. If the object has already been moved, its
;; forwarding address (found in the cdr position of the broken heart)
;; is substituted for the pointer in the pair being scanned.
;; Eventually, all accessible objects will have been moved and
;; scanned, at which point the scan pointer will overtake the free
;; pointer and the process will terminate.
;;

begin-garbage-collection
  (assign free (const 0))
  (assign scan (const 0))
  (assign old  (reg root))
  (assign relocate-continue (label reassign-root))
  (goto (label relocate-old-result-in-new))

reassign-root
  (assign root (reg new))
  (got (label gc-loop))
  
