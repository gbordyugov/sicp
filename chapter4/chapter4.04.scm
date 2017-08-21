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

(address ?name (Slummerville . ?address))


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

(and (supervisor ?supervisee ?supervisor)
     (not (job ?supervisor (computer ?job)))
     (job ?supervisor ?supervisor-job))


;;
;; exercise 4.57
;;

(rule (can-replace ?p1 ?p2)
      (and (or (and (job ?p1 ?job)
                    (job ?p2 ?job))
               (and (job ?p1 ?j1)
                    (job ?p2 ?j2)
                    (can-do-job ?j1 ?j2)))
           (not (same ?p1 ?p2))))

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
     (lisp-value > ?salaray-p2 ?salary-p1))


;;
;; exercise 4.58
;;

(rule (big-shot ?person ?division)
      (and (job ?person (?division . ?x))
           (or (not (supervisor ?person ?anyone))
               (and (supervisor ?person ?supervisor)
                    (not (job ?supervisor (?division . ?y)))))))


;;
;; exercise 4.59
;;

;;
;; a.
;;

(meeting ?team (Friday ?time))


;;
;; b.
;;

(rule (meeting-time ?person ?day-and-time)
      (or (meeting whole-company ?day-and-time)
          (and (meeting ?dept ?day-and-time)
               (job     ?person (?dept . ?x)))))

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

;; (2 3) next-to 4
;; 1 next-to (2 3)


;; 3 next-to 1
;; 2 next-to 1


;;
;; exercise 4.62
;;

(rule (last-pair (?a) (?a)))
(rule (last-pair (?x . ?rest) (?pair))
      (last-pair ?rest (?pair)))


;;
;; exercise 4.63
;;

(rule (grandfather ?gf ?gs)
      (and (son         ?gf ?f)
           (son         ?f  ?gs)))

(rule (son ?m ?s)
      (and (wife ?m ?w)
           (son  ?w ?s)))