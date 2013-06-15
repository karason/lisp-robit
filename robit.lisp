;robit.lisp

(load "/home/karason/quicklisp/setup.lisp")

(ql:quickload "cl-irc")

(defpackage :robit
    (:use :common-lisp :irc)
    (:export boot))

(in-package :robit)

(defparameter *nick* "rk[imposter]")

(defparameter *server* "irc.freenode.net")

(defparameter *channel* "#think")

(defun boot ()
    (defparameter *connection*
        (connect
            :nickname *nick*
            :server *server*)) 
    (join *connection* *channel*)
    (add-hook *connection* 'irc::irc-privmsg-message 'ping-hook)
    (ping-loop))

(defun say (message)
    (cond
        ((eq message 'nil) 'nil)
        (t (privmsg *connection* *channel* (string message)))))

(defun ping-hook (ping)
    (let
        ((current-ping
            (make-ping (get-universal-time) (source ping) (car (last (arguments ping))))))
        (log-ping current-ping)
        (say (evaluate-ping current-ping))))
 
(defun ping-loop ()
    (read-message-loop *connection*))

(defclass ping ()
    ((date :accessor ping-date
           :initarg :date)
     (nick :accessor ping-nick
           :initarg :nick)
     (message :accessor ping-message
              :initarg :message)))

(defun make-ping (date nick message)
    (make-instance 'ping :date date :nick nick :message message))

(defun log-ping (ping-object)
    (let 
        ((stream (open "/home/karason/robit/think.log" :direction :output :if-exists :append)))
        (princ (ping-date ping-object) stream)
        (princ " <" stream)
        (princ (ping-nick ping-object) stream)
        (princ "> " stream)
        (princ (ping-message ping-object) stream)
        (princ #\newline stream)
        (close stream)))

(defun evaluate-ping (ping-object)
    (evaluate-message (ping-message ping-object)))

(defun evaluate-message (message)
    (cond 
        ((search "(" message)
            (cond 
                ((search ")" message :from-end t) 
                    (write-to-string (ignore-errors (eval (read-from-string 
                        (subseq message (search "(" message) (+ 1 (search ")" message :from-end t))))))))
                (t nil)))
        (t nil)))
