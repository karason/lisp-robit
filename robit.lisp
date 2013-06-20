;;;; robit.lisp- an internet relay chat automaton defined in common lisp.

;;; Copyright (C) 2013 Chris Wallace and Ryan Karason
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


(in-package :robit)

;;; startup sequence- connect to a server
(defun boot ()
    (defparameter *connection*
        (connect
            :nickname *nick*
            :server *server*))
    (join *connection* *channel*)
    (say "ima robit.")
    (add-hook *connection* 'irc::irc-privmsg-message 'ping-hook)
    (ping-loop))

;;; say- allows creation of mesages
(defun say (message)
    (cond
        ((eq message 'nil) 'nil)
        (t
            (let
                ((current-ping
                    (make-ping (get-universal-time) *nick* message)))
                (log-ping current-ping)
                (privmsg *connection* *channel* message)))))

(defun ping-hook (ping)
    (let
        ((current-ping
            (make-ping (get-universal-time) (source ping) (car (last (arguments ping))))))
        (log-ping current-ping)
        (say (evaluate-ping current-ping))))

;;; the PING class
;;; 	Used for events and incoming messages
(defclass ping ()
    ((date :accessor ping-date
           :initarg :date)
     (nick :accessor ping-nick
           :initarg :nick)
     (message :accessor ping-message
              :initarg :message)))

;;; constructor
(defun make-ping (date nick message)
    (make-instance 'ping :date date :nick nick :message message))

;;; ping loop reads messages on the connection
(defun ping-loop ()
    (read-message-loop *connection*))

;;; create pretty output from a ping object
(defun prettify-ping (ping-object)
    (let
        ((date-list
            (multiple-value-list (decode-universal-time (ping-date ping-object)))))
        (concatenate 'string
            (let
                ((hour
                    (nth 2 date-list)))
                (cond
                    ((< hour 10)
                        (concatenate 'string
                        "0"
                        (write-to-string hour)))
                    (t (write-to-string hour))))
            ":"
            (let
                ((minute
                    (nth 1 date-list)))
                (cond
                    ((< minute 10) 
                        (concatenate 'string
                        "0"
                        (write-to-string minute)))
                    (t (write-to-string minute))))
            " <"
            (ping-nick ping-object)
            "> "
            (ping-message ping-object))))

(defun log-ping (ping-object)
    (let 
        ((stream 
            (open 
                (concatenate 'string 
                    *path*
                    "/logs/"
                    *channel*
                    ".log")
                :direction :output 
                :if-exists :append 
                :if-does-not-exist :create)))
        (princ (prettify-ping ping-object) stream)
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
