;robit.lisp
(defpackage :robit
    (:use :common-lisp))

(in-package :robit)

(defclass ping ()
    ((date :accessor ping-date
           :initarg :date)
     (nick :accessor ping-nick
           :initarg :nick)
     (message :accessor ping-message
              :initarg :message)))

(defun make-ping (date nick message)
    (make-instance 'ping :date date :nick nick :message message))

(defparameter current-ping 
    (make-ping (get-universal-time) 'ryankarason "hello world!"))

(defun pretty-print (ping-object)
    (princ (ping-date ping-object))
    (princ " <")
    (princ (ping-nick ping-object))
    (princ "> ")
    (princ (ping-message ping-object))
    (princ #\newline))

(pretty-print current-ping)

(defun evaluate-ping (ping-object)
    (evaluate-message (ping-message ping-object)))

(defun evaluate-message (message)
    (cond 
        ((search "(" message)
            (cond 
                ((search ")" message :from-end t) 
                    (eval (read-from-string 
                        (subseq message (search "(" message) (+ 1 (search ")" message :from-end t))))))
                (t nil)))
        (t nil)))

(setf current-ping 
    (make-ping (get-universal-time) 'ryankarason "hello robit, can you evaluate (+ 1 2 (- 3 (* 4 5))) for me?"))

(princ (evaluate-ping current-ping))
