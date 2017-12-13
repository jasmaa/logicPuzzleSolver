; ---- Lisp code starts here ---
;
;;;  Prolog in Lisp:  February 14th 2017 - February 18th 2017
;;;
;;;  From Peter Norvig's book: Chapter 11
;;; "Paradigms of Artificial Intelligence Programming"
;;;
;;; Unification, Prolog, Resolution, inference control strategies
;;;
;;; Posted:
; http://interweave-consulting.blogspot.co.uk/2017/02/a-simple-prolog-interpreter-in-lisp.html
;;;
;;;  Reminder: (how to load and access files)
;;;
; (load  "C:\\Users\\HP Owner\\Google Drive\\Lisp\\Prog-Prolog\\Prolog-in-Lisp.lisp")

;;;
;;; --- Unification ---
;;;
;;;  Bindings: a list of dotted pairs created by unify

(defconstant +fail+ nil "Indicates pat-match failure.")

(defconstant +no-bindings+ '((t . t))
"Indicates pat-match success but with no variables.")

(defun variable-p (x)    ; Symbol -> Bool
  "Is x a variable, a symbol beginning with '?'"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\? ))) 

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings) ))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list, remove (t . t)"
  (cons (cons var val) (if (equal bindings +no-bindings+) nil bindings)))

(defparameter *occurs-check* t "Should we do the occurs check? If yes, t")

(defun unify (x y &optional (bindings +no-bindings+))   ; -> Bindings
  "See if x and y match with given bindings"
  (cond ((eq bindings +fail+) +fail+)
        ((eql x y) bindings)
        ((variable-p x) (unify-variable x y bindings))
        ((variable-p y) (unify-variable y x bindings))
        ((and (consp x) (consp y))
         (unify (rest x) (rest y)
                (unify (first x) (first y) bindings) ) )
        (t +fail+) ) )

(defun unify-variable (var x bindings)              ; -> Bindings
  "Unify var with x, using (and maybe extending) bindings"
  (cond ((get-binding var bindings)
                  (unify (lookup var bindings) x bindings))
            ((and (variable-p x) (get-binding x bindings))
                  (unify var (lookup x bindings) bindings))
              ((and *occurs-check* (occurs-check var x bindings))
                  +fail+)
            (t (extend-bindings var x bindings))))

(defun occurs-check (var x bindings)             ; -> Bool
  "Does var occur anywhere 'inside x'? Returns t if it does (so fail)"
  (cond ((eq var x) t)
        ((and (variable-p x) (get-binding x bindings))
         (occurs-check var (lookup x bindings) bindings))
        ((consp x) (or (occurs-check var (first x) bindings)
                       (occurs-check var (rest x) bindings)))
        (t nil)))


(defun unifier (x y )
  "Return something that unifies with both x and y (or +fail+)"
  (subst-bindings (unify x y) x ) )


(defun subst-bindings (bindings x)   ; Bindings x Term -> Term
  "Substitute the value of variables in bindings into x,
taking recursively bound variables into account"
  (cond ((eql bindings +fail+) +fail+ )
        ((eql bindings +no-bindings+) x)
        ((and (variable-p x) (get-binding x bindings))
              (subst-bindings bindings (lookup x bindings) ))
        ((atom x) x)
        ( t (cons (subst-bindings bindings (car x))
                        (subst-bindings bindings (cdr x )) ) ) ) )


;;; -------------------------- Theorem Prover -----------------
;;;
;;; Clauses are represented as (head . body) cons cells: example clauses
;;;
;;;  ( (member ?item (?item . rest))) )                         ; fact
;;;  ( (member ?item (?x . ?rest)) . ((member ?item ?rest)))    ; rule


(defun clause-head (clause) (first clause))     ; Clause -> Literal

(defun clause-body (clause) (rest clause))      ; Clause -> Literal-list

;; Clauses are stored on the predicate's plist

(defun get-clauses (pred) (get pred 'clauses)) ; symbol -> Clause-list

(defun predicate (literal) (first literal))    ; Literal -> Symbol (predicate)

(defvar *db-predicates* nil
  "A list of all predicates stored in the database")

(defun replace-?-vars (exp)
  "Replace any ? within exp with a var of the form ?123"
  (cond ((eq exp '?) (gensym "?"))
        ((atom exp) exp)
        (t (cons (replace-?-vars (first exp))
                 (replace-?-vars (rest exp))) ))  )


(defun add-clause (clause)
  "Add a clause to the data base, indexed by head's predicate"
  ;; The predicate must be a non-variable symbol.
  (let* ((clause1 (replace-?-vars clause))
         (pred (predicate (clause-head clause1))))
    (assert (and (symbolp pred) (not (variable-p pred))))
    (pushnew pred *db-predicates*)
    (setf (get pred 'clauses)
          (append (get-clauses pred) (list clause1))) pred) )   ; changed nconc to append

; (setf clause '((P ?x ?y) . ((Q ?x ?y))))
; (add-clause clause)                       ; => P
; *db-predicates*                           ; => (P)
; (get 'P 'clauses)                         ; (((P ?X ?Y) (Q ?X ?Y)))

(defun show-all-clauses ()   ; *db-predicates*  -> Clause-list    (new function)
    "Retrieve all the clauses in the knowledge-base"
     (apply #'append (mapcar #'get-clauses *db-predicates*)))

(defun clear-db ()
  "Remove all clauses (for all predicates) from the database"
  (mapc #'clear-predicate *db-predicates*))

(defun clear-predicate (predicate)
  "Remove the clauses for a single predicate"
  (setf (get predicate 'clauses) nil) )

(defun test ()
  (clear-db)
  (setf *db-predicates* nil)
  (add-clause '((likes Kim Robin)))
  (add-clause '((likes Sandy Lee)))
  (add-clause '((likes Sandy Kim)))
  (add-clause '((likes Robin cats)))
  (add-clause '((likes Sandy ?x) (likes ?x cats)))
  (add-clause '((likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))
  (add-clause '((likes ?x ?x)))
  (format t "~&Predicate database *db-predicates* = ~a" *db-predicates*)
  (terpri)
  (pprint (show-all-clauses))
  (terpri)
  (prove '(likes Sandy ?who) +no-bindings+) )

(defun prove (goal b)    ; Literal x Bindings -> Bindings
  "Return a list of possible solutions to goal"
  (let ((kb-clauses (get-clauses (predicate goal))))  ; kb clauses which match goal
    (apply #'append (mapcar #'(lambda (kb-clause1)
                                (let* ((kb-clause       (rename-variables kb-clause1))
                                       (kb-clause-head  (clause-head kb-clause))
                                       (kb-clause-body  (clause-body kb-clause))
                                       (goal-bindings   (unify goal kb-clause-head b)))
                                  (prove-all kb-clause-body goal-bindings)))
                            kb-clauses)) ) )

(defun prove-all (goals goal-bindings) ; Literal-list x Bindings -> Bindings
  "Return a list of solutions to the conjunction of goals"
  (cond ((eql goal-bindings +fail+) +fail+)
        ((null goals) (list goal-bindings))
        (t  (let* ((next-bindings (prove (car goals) goal-bindings)))
              (apply #'append (mapcar #'(lambda (b) (prove-all (cdr goals) b))
                                      next-bindings)) ) ) ) )


(defun rename-variables (x)       ; clause -> clause (with vars renamed)
  "Replace all variables in x with new ones"
  (sublis (mapcar #'(lambda (var) (cons var (gensym (string var))))
                  (variables-in x))
          x))

;;; --- Example ---
#|
(add-clause '((likes Kim Robin)))
(add-clause '((likes Sandy Lee)))
(add-clause '((likes Sandy Kim)))
(add-clause '((likes Robin cats)))
(add-clause '((likes Sandy ?x) (likes ?x cats)))
(add-clause '((likes Kim ?x) (likes ?x Lee) (likes ?x Kim)))
(add-clause '((likes ?x ?x)))

(prove '(likes Sandy ?who) +no-bindings+)   ; =>

; (((?WHO . LEE))
;  ((?WHO . KIM))
;  ((#:?X846 . ROBIN)
;  (?WHO . #:?X846))
;  ((#:?X850 . CATS) (#:?X847 . CATS) (#:?X846 . SANDY) (?WHO . #:?X846))
;  ((#:?X855 . CATS) (#:?X846 . #:?X855) (?WHO . #:?X846))
;  ((?WHO . SANDY) (#:?X857 . SANDY)))
;
; - End Example
|#

;;; --- Prolog-like macros - defined also in Zebra-Puzzle ---
;
(defmacro <- (&rest clause)
  "Add a clause to the database"
  `(add-clause ',clause))

; (macroexpand-1 '(<- (likes Kim Robin)))    ; =>
; (ADD-CLAUSE (QUOTE ((LIKES KIM ROBIN))))   ; which is correct.

 (defmacro ?- (&rest goals) `(top-level-prove ',(replace-?-vars goals)))

; (macroexpand-1 '(?- goals))

;;; --- Prove removing spurious bindings ---

(defun variables-in (exp)
  "Return a list of all the variables in exp"
  (unique-find-anywhere-if #'variable-p exp))

(defun unique-find-anywhere-if (predicate tree &optional found-so-far)
  "Return a list of leaves of tree satisfying predicate, with duplicates removed"
  (if (atom tree)
      (if (funcall predicate tree) (adjoin tree found-so-far) found-so-far)
      (unique-find-anywhere-if predicate (first tree)
                            (unique-find-anywhere-if predicate (rest tree) found-so-far)
 )))

(defun top-level-prove (goals)
  "Prove the goals, and print variables readably"
  (show-prolog-solutions (variables-in goals) (prove-all goals +no-bindings+)))

(defun show-prolog-solutions (vars solutions)
  "Print the variables in each of the solutions"
  (if (null solutions)
      (format t "~&No.")
      (mapc #'(lambda (solution) (show-prolog-vars vars solution)) solutions))
  (values))

(defun show-prolog-vars (vars bindings)
  "Print each variable with its binding"
  (if (null vars)
      (format t "~&Yes")
    (dolist (var vars)
      (format t "~&~a = ~a" var (subst-bindings bindings var))))
  (princ ";"))

;
;;; --- Zebra Puzzle  ---
; (load  "C:\\Users\\HP Owner\\Google Drive\\Lisp\\Prog-RTP\\Zebra-Puzzle.lisp")
;
;;; --- Zebra Puzzle  ---
;
#| Begin comments

Here is an example of something Prolog is very good at: a logic puzzle. There are
fifteen facts, or constraints, in the puzzle:

1. There are five houses in a line, each with an owner, a pet, a cigarette, a drink,
and a color.
2. The Englishman lives in the red house.
3. The Spaniard owns the dog.
4. Coffee is drunk in the green house.
5. The Ukrainian drinks tea.
6. The green house is immediately to the right of the ivory house.
7. The Winston smoker owns snails.
8. Kools are smoked in the yellow house.
9. Milk is drunk in the middle house.
10. The Norwegian lives in the first house on the left.
11. The man who smokes Chesterfields lives next to the man with the fox.
12. Kools are smoked in the house next to the house with the horse.
13. The Lucky Strike smoker drinks orange juice.
14. The Japanese smokes Parliaments.
15. The Norwegian lives next to the blue house.

The questions to be answered are: who drinks water and who owns the zebra?

End comments
|#

(<- (member ?item (?item . ?rest )))
(<- (member ?item (?x . ?rest) ) (member ?item ?rest))

(<- (nextto ?x ?y ?list) (iright ?x ?y ?list))
(<- (nextto ?x ?y ?list) (iright ?y ?x ?list))

(<- (iright ?left ?right (?left ?right . ?rest)))
(<- (iright ?left ?right (?x . ?rest)) (iright ?left ?right ?rest))
(<- (= ?x ?x))

;; Each house is of the form:
;; (house nationality pet cigarette drink house-color)
;; ?h is the variable representing the list of the five houses

(<- (zebra ?h ?w ?z)

  (= ?h ((house norwegian ? ? ? ?) ? (house ? ? ? milk ?) ? ? ) ) 		  ; 1, 10, 9
  (member (house englishman ? ? ? red) ?h)                                ; 2
  (member (house spaniard dog ? ? ?) ?h)                                  ; 3
  (member (house ? ? ? coffee green) ?h)                                  ; 4
  (member (house ukrainian ? ? tea ?) ?h)                                 ; 5
  (iright (house ? ? ? ? ivory)                                           ; 6
          (house ? ? ? ? green) ?h)
  (member (house ? snails winston ? ?) ?h)                                ; 7
  (member (house ? ? kools ? yellow) ?h)                                  ; 8
  (nextto (house ? ? chesterfield ? ?)                                  ; 11
          (house ? fox ? ? ?) ?h)
  (nextto (house ? ? kools ? ?)                                          ; 12
          (house ? horse ? ? ?) ?h)
  (member (house ? ? luckystrike orange-juice ?) ?h)             	     ; 13
  (member (house japanese ? parliaments ? ?) ?h)                       ; 14
  (nextto (house norwegian ? ? ? ?)                                              ; 15
          (house ? ? ? ? blue) ?h)

;; Now for the questions:

   (member (house ?w ? ? water ?) ?h)                                        ; Q1
   (member (house ?z zebra ? ? ?) ?h) )                                        ; Q2

; Here's the query

;  (?- (zebra ?houses ?water-drinker ?zebra-owner))

; .. and here's the result

; ?HOUSES = ((HOUSE NORWEGIAN FOX KOOLS WATER YELLOW)
; (HOUSE UKRAINIAN HORSE CHESTERFIELD TEA BLUE)
; (HOUSE ENGLISHMAN SNAILS WINSTON MILK RED)
; (HOUSE SPANIARD DOG LUCKYSTRIKE ORANGE-JUICE IVORY)
; (HOUSE JAPANESE ZEBRA PARLIAMENTS COFFEE GREEN))
; ?WATER-DRINKER = NORWEGIAN
; ?ZEBRA-OWNER = JAPANESE.
; No.
;
; ------------------------------------------------------------------------------------------------------------
; The Zebra Puzzle is at:
;
;  http://interweave-consulting.blogspot.co.uk/2017/02/the-zebra-puzzle-prolog-in-lisp.html
;
;                                            ----------------- End ------------------