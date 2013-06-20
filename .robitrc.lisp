;;;; .robitrc.lisp- the initilization file for robit.lisp

;;; load quicklisp system
(load "/home/chris/quicklisp/setup.lisp")

;;; load cl-irc library
(ql:quickload "cl-irc")

;;; define :robit package
(defpackage :robit
    (:use common-lisp :irc)
    (:export boot))

;;; move scope to :robit package
(in-package :robit)

;;; set path directory for robit
(defparameter *path* "/home/chris/workspace/robit/")

;;; set nick of robit
(defparameter *nick* "robit-czl")

;;; set server of robit
(defparameter *server* "irc.freenode.net")

;;; set channel of robit
(defparameter *channel* "#think")
