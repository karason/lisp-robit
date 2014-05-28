;;;; robit.lisp— an internet relay chat automaton defined in common lisp.

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

;;; change scope to :robit namespace
(in-package :robit)

;;; startup sequence- connect to a server
(defun boot ()
    (defparameter *connection*
        (connect
            :nickname *nick*
            :server *server*))
    (cond
        (*password*
            (privmsg *connection* "nickserv"
                (concatenate 'string
                    "identify "
                    *password*)))
        (t 'nil))
    (labels 
        ((join-all (channels)
            (cond
                ((eq channels 'nil) 'nil)
                (t
                    (join *connection* (car channels))
                    (join-all (cdr channels))))))
        (join-all *channels*))
    (say-all *channels* "ima robit.")
    (add-hook *connection* 'irc::irc-privmsg-message 'ping-hook)
    (ping-loop))

;;; say— echo message to channel 
(defun say (channel message)
    (cond
        ((eq message 'nil) 'nil)
        (t
            (let
                ((current-ping
                    (make-ping (get-universal-time) channel *nick* message)))
                (log-ping current-ping)
                (privmsg *connection* channel message)))))

;;; say all— echo message to list of channels
(defun say-all (channels message)
    (cond
        ((eq channels 'nil) 'nil)
        (t
            (say (car channels) message)
            (say-all (cdr channels) message))))

;;; act— echo action to channel
(defun act (channel action)
    (privmsg *connection* channel (format nil "~A~A~A~A" #\Soh "ACTION " action #\Soh)))

;;; ping-hook— create a log and response hook for incoming ping
(defun ping-hook (ping)
    (let
        ((current-ping
            (make-ping 
                (get-universal-time) 
                (cond 
                    ((equal (first (arguments ping)) *nick*) (source ping)) 
                    (t (first (arguments ping)))) 
                (source ping) 
                (car (last (arguments ping))))))
        (log-ping current-ping)
        (say (ping-channel current-ping) (evaluate-ping current-ping))))

;;; ping-loop— read messages on the connection
(defun ping-loop ()
    (read-message-loop *connection*))

;;; the PING class
;;;     used for events and incoming messages
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

;;; make-ping— constructor for ping object
(defun make-ping (date channel nick message)
    (make-instance 'ping :date date :channel channel :nick nick :message message))

;;; number-to-string— convert number to string with padding
(defun number-to-string (digits)
    (cond
        ((< digits 10)
            ;; pad single digit with leading zero
            (concatenate 'string "0" (write-to-string digits)))
        (t (write-to-string digits))))

;;; prettify-ping— create pretty output from a ping object
(defun prettify-ping (ping-object)
    (let
        ((date-list
            (multiple-value-list (decode-universal-time (ping-date ping-object)))))
        (concatenate 'string
            (number-to-string (nth 2 date-list)) ; hours
            ":"
            (number-to-string (nth 1 date-list)) ; minutes
            ":"
            (number-to-string (nth 0 date-list)) ; seconds
            " <"
            (ping-nick ping-object)
            "> "
            (ping-message ping-object))))

;;; prettify-date— create pretty output from universal timestamp
(defun prettify-date (date)
    (let
        ((date-list
            (multiple-value-list (decode-universal-time date))))
        (concatenate 'string 
            (write-to-string (nth 5 date-list)) ; year
            "-"
            (number-to-string (nth 4 date-list)) ; month
            "-"
            (number-to-string (nth 3 date-list))))) ; day

;;; get-log-path— get full path for logging
(defun get-log-path (ping-object)
    (concatenate 'string
        *path*
        "/logs/"
        (ping-channel ping-object)
        "/"
        (prettify-date (ping-date ping-object))
        ".log"))

;;; log-ping— write a ping object to log file
(defun log-ping (ping-object)
    (ensure-directories-exist
        (concatenate 'string 
            *path* "/logs/" (ping-channel ping-object) "/"))
    (let
        ((stream
            (open (get-log-path ping-object)
                :direction :output
                :if-exists :append
                :if-does-not-exist :create)))
        (princ (prettify-ping ping-object) stream)
        (princ #\newline stream)
        (close stream)))

;;; evaluate-ping— evaluate a ping object
(defun evaluate-ping (ping-object)
    (evaluate-message (ping-channel ping-object) (ping-nick ping-object) (ping-message ping-object)))

;;; evaluate-message— evaluate a message
(defun evaluate-message (channel nick message)
    (cond 
        ((search "(" message)
            (cond
                ((search ")" message :from-end t)
                    (write-to-string 
                        (ignore-errors (eval 
                            ;`(let ((*ping-channel* ,channel) (*ping-nick* ,nick)) 
                            ;,(read-from-string (subseq message (search "(" message) (+ 1 (search ")" message :from-end t))))))))
                            (read-from-string
                                (concatenate 'string
                                    "(let ((*ping-channel* "
                                    (write-to-string channel)
                                    ") (*ping-nick* "
                                    (write-to-string nick)
                                    ")) "
                                    (subseq message (search "(" message) (+ 1 (search ")" message :from-end t)))
                                    ")"))))))
                (t nil)))
        (t nil)))

;;; auto-load all accessory modules
(labels 
    ((load-all (accessories)
        (cond
            ((eq accessories 'nil) 'nil)
            (t
                (load (concatenate 'string *path* "/modules/" (car accessories)))
                (load-all (cdr accessories))))))
    (in-package :cl-user)
    (load-all *accessories*))

;;; for repl
(setq custom:*prompt-body* "")

;;; print robit header to screen
(progn
    (format t "~%")
    (format t "RRR   00  BBB  IIII TTTT~%")
    (format t "R  R O  O B  B  II   TT~%")
    (format t "RRR  O  O BBB   II   TT~%")
    (format t "R R  O  O B  B  II   TT~%")
    (format t "R  R  OO  BBB  IIII  TT~%")
    (format t "~%"))
