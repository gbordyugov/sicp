;;
;; the data base
;;
;; todo: how to add them in a nice manner?
;;
;;

(define db
  '((address     (Bitdiddle Ben)   (Slumerville (Ridge Road) 10))
    (job         (Bitdiddle Ben)   (computer wizard))
    (salary      (Bitdiddle Ben)   60000)

    (address     (Hacker Alyssa P) (Cambridge (Mass Ave) 78))
    (job         (Hacker Alyssa P) (computer programmer))
    (salary      (Hacker Alyssa P) 40000)
    (supervisor  (Hacker Alyssa P) (Bitdiddle Ben))

    (address     (Fect Cy D)       (Cambridge (Ames Street) 3))
    (job         (Fect Cy D)       (computer programmer))
    (salary      (Fect Cy D)       35000)
    (supervisor  (Fect Cy D)       (Bitdiddle Ben))

    (address     (Tweakit Lem E)   (Boston (Bay State Road) 22))
    (job         (Tweakit Lem E)   (computer technician))
    (salary      (Tweakit Lem E)   25000)
    (supervisor  (Tweakit Lem E)   (Bitdiddle Ben))

    (address     (Reasoner Louis)  (Slumerville (Pine Tree Road) 80))
    (job         (Reasoner Louis)  (computer programmer trainee))
    (salary      (Reasoner Louis)  30000)
    (supervisor  (Reasoner Louis)  (Hacker Alyssa P))

    (address     (Warbucks Oliver) (Swellesley (Top Heap Road)))
    (job         (Warbucks Oliver) (administration big wheel))
    (salary      (Warbucks Oliver) 150000)
    (supervisor  (Bitdiddle Ben)   (Warbucks Oliver))

    (address     (Scrooge Eben)    (Weston (Shady Lane) 10))
    (job         (Scrooge Eben)    (accounting chief accountant))
    (salary      (Scrooge Eben)    750000)
    (supervisor  (Scrooge Eben)    (Warbucks Oliver))

    (address     (Cratchet Robert)  (Allston (N Harvard Streem) 16))
    (job         (Cratchet Robert)  (accounting scrivener))
    (salary      (Cratchet Robert)  180000)
    (supervisor  (Cratchet Robert)  (Scrooge Eben))

    (address     (DeWitt Aull)      (Slumerville (Onion Square) 5))
    (job         (DeWitt Aull)      (administration secretary))
    (salary      (DeWitt Aull)      25000)
    (supervisor  (DeWitt Aull)      (Warbucks Oliver))

    (can-do-job (computer wizard)          (computer programmer))
    (can-do-job (computer wizard)          (computer technician))
    (can-do-job (computer programmer)      (computer programmer trainee))
    (can-do-job (administration secretary) (administration big wheel))))

(define (load-db db)
  (if (null? db)
    'ok
    (begin
      (add-rule-or-assertion! (car db))
      (load-db (cdr db)))))

(load "query-interpreter.scm")

(trace apply-a-rule)
(break apply-a-rule)
(trace qeval)
(break qeval)

(load-db db)
(query-driver-loop)
