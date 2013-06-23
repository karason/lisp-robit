(defparameter safe-commands
  (list '+ '- '* '/))

(defmacro is-safe (sexp)
  ;; check if nil
  (if sexp
      (if (atom (car sexp))
          (if (member (car sexp) safe-commands)
             (is-safe (cdr sexp))
            nil)
        (and (is-safe (car sexp)) (is-safe (cdr sexp))))
    ;; if nil it's safe 
    T))

(defmacro safe-eval (sexp)
  (if (is-safe? sexp) (eval sexp) ("No hax allowed")) 
