#|
Notes:

lists copied to hash are the same list

Method:
-loads in contraints
-builds table of possible links

1. disassociates using constraints list
2. uses POE if definite association exists
3. syncs the buckets if a definite association exists

|#

(let 	(
			(num-features 3)
			(num-elements 4)
			(features nil)
			(constraints nil)
			(constraint-graveyard nil)
			(possible nil)
		)

		; --- temp: set vars ---
		(setq num-features 3)
		(setq num-elements 4)
		(setq features '(
			(GERTIE HERBERT MIRIAM WALLACE)
			(AL BE PE SL)
			(1928 1929 1932 1935)
		))
		(setq constraints '(
			(HERBERT BE)
			(SL GERTIE)
			(SL HERBERT)
			(BE 1935)
			(1932 AL)
			(1932 BE)
			(PE 1928)
			(PE 1932)
			(PE 1935)
			(WALLACE BE)
			(WALLACE PE)
			(WALLACE SL)
		))
		; --- temp: set vars ---


		; Build list of possibilities
		(dotimes (i num-features nil)
			(dotimes (j num-features nil)
				(if (not (equalp j i))
					(let ((table nil))
						(setq table (make-hash-table))
						(dotimes (k num-elements nil)
							(setf (gethash (nth k (nth i features)) table) (nth j features))
						)
						(setq possible (cons table possible))
					)
				)
			)
		)

		; Run thru constraints
		(loop 
			(if (<= (length constraints) 0) (return))
			(setq c (car constraints))
			
			; insert alg here
			(print c)
			
			(setq constraint-graveyard (cons (car constraints) constraint-graveyard))
			(setq constraints (cdr constraints))
		)
		
		;(print possible)
		

)