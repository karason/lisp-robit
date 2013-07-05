(defparameter arithm
  (list '+ '- '* '/))

(defun is-safe? (sexp safe-commands)
  (cond 
	((eq sexp nil)
		T)

	((atom sexp) 
	 (member sexp safe-commands))

	((atom (car sexp))
		(and (member (car sexp) safe-commands) 
			 (is-safe? (cdr sexp))) )

	(T 
	  (and (is-safe? (car sexp))
		   (is-safe? (cdr sexp))))))
	 
