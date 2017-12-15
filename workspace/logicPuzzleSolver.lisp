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

(defun print-h(p-set)
	(print "--- START ---")
	(print "---")
	(dolist (p p-set)
		(maphash #'(lambda (k v) (print (list k v))) p)
		(print "---")
	)
)

(defun get-keys(h)
	(let ((keys nil))
		(maphash #'(lambda (k v) (setq keys (cons k keys))) h)
		keys
	)
)

(let 	(
			(num-features 3)
			(num-elements 4)
			(features nil)
			(constraints nil)
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
			
			; loop for disassociations
			(dolist (p possible)
				(if (and (gethash (car c) p) (member (nth 1 c) (gethash (car c) p)))
					(setf (gethash (car c) p) (remove (nth 1 c) (gethash (car c) p)))
					
					(if (and (gethash (nth 1 c) p) (member (car c) (gethash (nth 1 c) p)))
						(setf (gethash (nth 1 c) p) (remove (car c) (gethash (nth 1 c) p)))
					)
				)
			)

			; check for POE
			(dolist (p possible)
				(dolist (feature (get-keys p))
					(if (equalp 1 (length (gethash feature p)))
						(dolist (f (get-keys p))
							(when (not(equalp f feature))
								(setf (gethash f p) (remove (car (gethash feature p)) (gethash f p)))
							)
						)
					)
				)
			)
			
			

			(setq constraints (cdr constraints))
		)

		
		(print-h possible)
		(print "-- algorithm ended --")
		nil
)