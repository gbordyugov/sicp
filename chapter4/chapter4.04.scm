;;
;; 4.4 Logic Programming
;;

;;
;; the aim is to implement a _query_ language
;;
;; use smth similar for our KPI queries?
;;




;;
;; exercise 4.55
;;

;;
;; 1. all people supervised by Ben Bitdiddle
;;

(supervisor ?name (Bitdiddle Ben))

;;
;; 2. the names and jobs of all people in the accounting division
;;

(job ?name (accounting . ?job))

;;
;; 3. the names and addresses of all ppl who live in Slumerville
;;

(address ?name (Slumerville . ?address))

;;
;; Compound queries
;;

(and (job ?person (computer programmer))
     (address ?person ?where))

(supervisor ?x (Bitdiddle Ben))

(or (supervisor ?x (Bitdiddle Ben))
    (supervisor ?x (Hacker Allysa P)))

;;
;; exercise 4.56
;;

;;
;; a. the names of all people who are supervised by BB together with
;; their addresses

(and (supervisor ?name (Bitdiddle Ben))
     (address    ?name ?address))

;;
;; b. all people whose salary is less than BB's together with their
;; salary and BB's salary
;;

(and (salary (Bitdiddle Ben) ?bb-salary)
     (salary ?name           ?salary)
     (lisp-value < ?salary ?bb-salary))

;;
;; c. all people who are supervised by someone who is not in the
;; computer division, together with the supervisor's name and job
;;

(and (job ?who ?job)
     (not (job ?who (computer . ?res))))

(and (supervisor ?supervisee ?supervisor)
     (not (job ?supervisor (computer . ?job)))
     (job ?supervisor ?supervisor-job))

;; interestingly enough, ?job is not calculated (not needed?)
;; consistent with Eli Bendersky's solution

(assert! (rule (same ?x ?x)))

(assert!
  (rule (lives-near ?p1 ?p2)
        (and (address ?p1 (?town . ?r1))
             (address ?p2 (?town . ?r2))
             (not (same ?p1 ?p2)))))

(and (lives-near ?x ?y)
     (address ?x ?add1)
     (address ?y ?add2))

(lives-near ?x (Bitdiddle Ben))


;;
;; exercise 4.57
;;

(assert!
  (rule (can-replace ?p1 ?p2)
        (and (or (and (job ?p1 ?job)
                      (job ?p2 ?job))
                 (and (job ?p1 ?j1)
                      (job ?p2 ?j2)
                      (can-do-job ?j1 ?j2)))
             (not (same ?p1 ?p2)))))

;;
;; a. all people who can replace Cy D. Fect
;;

(can-replace ?x (Fect Cy D))

;;
;; b. all people who can replace someone who is being paid more than
;; they are, together with they salaries
;;

(and (can-replace ?p1 ?p2)
     (salary      ?p1 ?salary-p1)
     (salary      ?p2 ?salary-p2)
     (lisp-value > ?salary-p2 ?salary-p1)) ;; found the typo!


;;
;; exercise 4.58
;;

(assert!
  (rule (big-shot ?person ?division)
        (and (job ?person (?division . ?x))
             (or (not (supervisor ?person ?anyone))
                 (and (supervisor ?person ?supervisor)
                      (not (job ?supervisor (?division . ?y))))))))

(big-shot ?who ?where)

(big-shot ?person computer)


;;
;; exercise 4.59
;;

(assert! (meeting accounting     (Monday     9am)))
(assert! (meeting administration (Monday    10am)))
(assert! (meeting computer       (Wednesday 10am)))
(assert! (meeting administration (Friday     1pm)))

(assert! (meeting whole-company  (Wednesday     4pm)))


;;
;; a.
;;

(meeting ?team (Friday ?time))


;;
;; b.
;;

(assert!
  (rule (meeting-time ?person ?day-and-time)
        (or (meeting whole-company ?day-and-time)
            (and (meeting ?dept ?day-and-time)
                 (job     ?person (?dept . ?x))))))

(meeting-time (hacker alyssa p) ?time)

;;
;; c.
;;

(and (meeting-time (Hacker Allysa P) (Wednesday . ?time))
     (meeting ?dept (Wednesday . ?time)))


;;
;; exercise 4.60
;;

;;
;; a rotation of a pair still satisfies the `lives-near` rule
;;
;; yes, by additionaly implying an ordering on pairs
;;


;;
;; exercise 4.61
;;

(assert!
  (rule (?x next-to ?y in (?x ?y . ?u))))
(assert!
  (rule (?x next-to ?y in (?v . ?z))
        (?x next-to ?y in ?z)))

(?x next-to ?y in (1 (2 3) 4))

(?x next-to  1 in (2 1 3 1))

;; (2 3) next-to 4
;; 1 next-to (2 3)


;; 3 next-to 1
;; 2 next-to 1


;;
;; exercise 4.62
;;

(assert!
  (rule (last-pair (?a) (?a))))

(assert!
  (rule (last-pair (?x . ?rest) (?pair))
        (last-pair ?rest (?pair))))

(last-pair (3) ?x)

(last-pair (1 2 3) ?x)

(last-pair (1 ?x) (3))

;; this one doesn't have a unique solution
;; (last-pair ?x (3))


;;
;; exercise 4.63
;;

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Enoch Mehujael))
(assert! (son Mehujael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))

(assert!
  (rule (grandfather ?gf ?gs)
        (and (son         ?gf ?f)
             (son         ?f  ?gs))))

(assert!
  (rule (son ?m ?s)
        (and (wife ?m ?w)
             (son  ?w ?s))))

(son ?f ?s)

(grandfather ?gf ?gs)



;;
;; exercise 4.64
;;

(rule (outranked-by ?staff-person ?boss)
      (or (supervisor ?staff-person ?boss)
          (and (outranked-by ?middle-manager ?boss)
               (supervisor ?staff-person ?middle-manager))))

;;
;; the same problem as with Mickey and Minnie in the text:
;;
;; (outranked-by (Bitdiddle Ben) ?who)) is unified with the conclusion
;; of the rule above, binding ?staff-person to (Bitdiddle Ben) and
;; ?who to ?boss
;;
;; the first leg of or works findind a binding for ?boss and then
;; (outranked-by ?middle-manager ?boss) kicks in, trying to find
;; ?middle-manager
;;
;; once a ?middle-manager has been found, it would again try to find
;; another ?boss
;;
;; the order of arguments to the `and` operator is important!
;;

;; my alibi commit


;;
;; exercise 4.65
;; 
;; because the dude is listed four times!
;;


;;
;; exercise 4.66
;;
;; Ben has just realized that hes functionality is lacking a
;; uniqueness check - duplicates should be weeded from the resulting
;; stream of frames
;;


;;
;; exercise 4.67
;;
;; I'm not there yet, skipped
;;


;;
;; exercise 4.68
;;

(assert!
  (rule (append-to-form () ?y ?y)))
(assert!
  (rule (append-to-form (?u . ?v) ?y (?u . ?z))
        (append-to-form ?v ?y ?z)))

(append-to-form (a b) (c d) ?z)

(append-to-form (a b) ?y (a b c d))

(assert!
  (rule (reverse () ())))
(assert!
  (rule (reverse ?a ?b)
        (and (append-to-form (?cara) ?cdra  ?a)
             (append-to-form  ?carb (?cara) ?b)
             (reverse ?cdra ?carb))))

(reverse (1 2 3) ?x) ;; this one produces an infinite loop

(reverse ?x (1 2 3)) ;; this one is working

;;
;; exercise 4.69
;;

(assert! (son Adam Cain))
(assert! (son Cain Enoch))
(assert! (son Enoch Irad))
(assert! (son Enoch Mehujael))
(assert! (son Mehujael Lamech))
(assert! (wife Lamech Ada))
(assert! (son Ada Jabal))
(assert! (son Ada Jubal))
(assert!
  (rule (ends-in-grandson (grandson))))
(assert!
  (rule (ends-in-grandson (?head . ?rest))
        (ends-in-grandson ?rest)))
(assert!
  (rule ((great . ?rel) ?x ?y)
        (and (ends-in-grandson ?rel)
             (?rel ?z ?y)
             (son ?x ?z))))

(append-to-form (a b) (c d)  ?z)
(append-to-form (a b)  ?y   (a b c d))
(append-to-form  ?x   (c d) (a b c d))

((grandson) ?g ?ggs)

((great grandson) ?g ?ggs)
