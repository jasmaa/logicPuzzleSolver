
(setq table (make-hash-table))

(setq test-list '(a b c))

(setf (gethash 'one table) (copy-list test-list))
(setf (gethash 'two table) 2)

(cons table test-list)

(setq list-1 '(a b c))
(setq list-2 '(a c))

(dolist (i list-1)
	(print i)
)

(intersection list-1 list-2)