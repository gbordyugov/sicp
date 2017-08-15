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
