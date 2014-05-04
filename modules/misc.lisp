;;;; misc.lisp

(defun robit ()
    "https://github.com/karason/lisp-robit")

(defun beep () 'boop)
(defun boop () 'beep)

(defun help (&optional query)
    (cond
        ((eq query 'nil) 
            "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")
        (t
            (concatenate 'string "http://www.xach.com/clhs?q=" (write-to-string query)))))
