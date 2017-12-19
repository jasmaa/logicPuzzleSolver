#|
Notes:

Method:
  -loads in constraints
  -builds table of possible links
  -loops the following until the puzzle is solved
    1. disassociates specified elements using constraints list
    2. uses POE if a definite association exist
    3. syncs the buckets if a definite association exists

|#

; prints out possible list for debug
(defun print-h(p-set)
	(print "--- --- ---")
	(dolist (p p-set)
		(maphash #'(lambda (k v) (print (list k v))) p)
		(print "--- --- ---")
	)
)

; prints results nicely
(defun print-res(p-set features)
	(dolist (f (car features))
		(let ((f-list nil))
			(setq f-list (cons f f-list))
			(dolist (bucket p-set)
				(if (not(null(gethash f bucket))) 
					(setq f-list (append f-list (gethash f bucket)))
				)
			)
			(print f-list)
		)
	)
)

; gets keys in a hashset
(defun get-keys(h)
	(let ((keys nil))
		(maphash #'(lambda (k v) (setq keys (cons k keys))) h)
		keys
	)
)

; Main
(defun main()
	(let 	(
				(num-features -1)
				(num-elements -1)
				(features nil)
				(constraints nil)
				(possible nil)
			)

			; Read in files
			(with-open-file (stream "<insert puzzle path here>")
				(setq num-features (read stream))
				(setq num-elements (read stream))

				; read features
				(dotimes (i num-features)
					(let ((feature-list nil))
						(dotimes (j num-elements)
							(setq feature-list (cons (read stream) feature-list))
						)
						(setq features (cons feature-list features))
					)
				)

				; read constraints
				(loop
					(let ((c0 nil) (c1 nil))

						(setq c0 (read stream nil nil))
						(setq c1 (read stream nil nil))
						(if (null c0) (return))
						
						(setq constraints (cons (list c0 c1) constraints))
					)
				)
			)


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

				; check for process of elimination
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
				
				
				; bucket syncing between feature and element
				(dolist (p possible)
					(dolist (feature (get-keys p))
						; detect a definite association
						(if (equalp 1 (length (gethash feature p)))
							(let ((element (car (gethash feature p))))
								; a
								(dolist (p1 possible)
									(if (gethash feature p1)
										; b
										(dolist (p2 possible)
											(if (gethash element p2)
												(let ((intersect nil))
													(setq intersect (intersection (gethash feature p1) (gethash element p2)))
													(when (> (length intersect) 0)
														(setf (gethash feature p1) intersect)
														(setf (gethash element p2) intersect)
													)
												)
											)
										)
									)
								)
							)
						)
					)
				)

				; Removes applied constraint
				(setq constraints (cdr constraints))

				; Checks if done
				(let ((counter 0))
					(dolist (p possible)
						(dolist (f (get-keys p))
							(setq counter (+ counter (length (gethash f p))))
						)
					)
					; Feed in dummy commands until solved
					(if (not(equalp counter (* num-features (1- num-features) num-elements)))
						(setq constraints (append constraints (list '(!DUMMY THIS-LINE-MAKES-THE-PROGRAM-WORK))))
					)
				)
			)

			; Prints results
			(print-res possible features)

			nil
	)
)
