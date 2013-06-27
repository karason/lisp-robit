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
    (labels 
        ((join-all (channels)
            (cond
                ((eq channels 'nil) 'nil)
                (t
                    (join *connection* (car channels))
                    (join-all (cdr channels))))))
        (join-all *channels*))
    ;(say "ima robit.")
    (add-hook *connection* 'irc::irc-privmsg-message 'ping-hook)
    (ping-loop))

;;; say- echo message to channel 
(defun say (channel message)
    (cond
        ((eq message 'nil) 'nil)
        (t
            (let
                ((current-ping
                    (make-ping (get-universal-time) channel *nick* message)))
                (log-ping current-ping)
                (privmsg *connection* channel message)))))

;;; ping hook- create a log and response hook for incoming ping
(defun ping-hook (ping)
    (let
        ((current-ping
            (make-ping (get-universal-time) (first (arguments ping)) (source ping) (car (last (arguments ping))))))
        (log-ping current-ping)
        (say (ping-channel current-ping) (evaluate-ping current-ping))))

;;; ping loop- read messages on the connection
(defun ping-loop ()
    (read-message-loop *connection*))

;;; the PING class
;;; 	Used for events and incoming messages
(defclass ping ()
    ((date 
        :accessor ping-date
        :initarg :date)
     (channel 
        :accessor ping-channel
        :initarg :channel)
     (nick 
        :accessor ping-nick
        :initarg :nick)
     (message 
        :accessor ping-message
        :initarg :message)))

;;; make ping- constructor for ping object
(defun make-ping (date channel nick message)
    (make-instance 'ping :date date :channel channel :nick nick :message message))

;;; prettify ping- create pretty output from a ping object
(defun prettify-ping (ping-object)
    (let
        ((date-list
            (multiple-value-list (decode-universal-time (ping-date ping-object)))))
        (concatenate 'string
            (let
                ((hour
                    (nth 2 date-list)))
                (cond
                    ;; if there's a single digit hour- make it double
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
                    ;; if there's a single digit minute- make it double
                    ((< minute 10)
                        (concatenate 'string
                            "0"
                            (write-to-string minute)))
                    (t (write-to-string minute))))
            ":"
            (let
                ((sec
                    (nth 0 date-list)))
                (cond
                    ;; if there's a single digit second- make it double
                    ((< sec 10)
                        (concatenate 'string
                            "0"
                            (write-to-string sec)))
                    (t (write-to-string sec))))
            " <"
            (ping-nick ping-object)
            "> "
            (ping-message ping-object))))

(defun prettify-date (date)
    (let
        ((date-list
            (multiple-value-list (decode-universal-time date))))
        (concatenate 'string 
            (write-to-string (nth 5 date-list))
            "-"
            (let
                ((month
                    (nth 4 date-list)))
                (cond
                    ;; if there's a single digit month- make it double
                    ((< month 10) 
                        (concatenate 'string
                            "0"
                            (write-to-string month)))
                    (t (write-to-string month))))
            "-"
            (let
                ((day
                    (nth 3 date-list)))
                (cond ;; if there's a single digit day- make it double
                    ((< day 10) 
                        (concatenate 'string
                            "0"
                            (write-to-string day)))
                    (t (write-to-string day)))))))

(defun get-log-path (ping-object)
    (concatenate 'string
        *path*
        "/logs/"
        (ping-channel ping-object)
        "/"
        (prettify-date (ping-date ping-object))
        ".log"))

(defun log-ping (ping-object)		; would we consider moving all logging functions
    (let							; to another file?
        ((stream                    ; -we would consider.
            (open (get-log-path ping-object)
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
