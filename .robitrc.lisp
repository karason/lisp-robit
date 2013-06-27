;;;; .robitrc.lisp- the initilization file for robit.lisp

;;; load quicklisp system
(load "/home/karason/quicklisp/setup.lisp")

;;; load cl-irc library
(ql:quickload "cl-irc")

;;; define :robit package
(defpackage :robit
    (:use common-lisp :irc)
    (:export boot))

;;; move scope to :robit package
(in-package :robit)

;;; set path directory for robit
(defparameter *path* "/home/karason/robit/")

;;; set nick of robit
(defparameter *nick* "robit")

;;; set server of robit
(defparameter *server* "irc.freenode.net")

;;; set channels of robit
(defparameter *channels* 
    (list "#think"))
