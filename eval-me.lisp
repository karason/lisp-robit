;EVAL-ME.LISP: create configuration file for ROBIT.LISP

(princ "Path of robit? ")
(defparameter *robit-path* 
    (read-line))

;stream for configuration file
(defparameter stream
    (open
        (concatenate 'string 
            *robit-path*
            "/.robitrc")
        :direction :output
        :if-exists :new-version
        :if-does-not-exist :create))

(princ ";;;; robitrc.lisp- the initialization file for robit.lisp" stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; suppess output of load" stream)
(princ #\newline stream)
(princ '(setq *load-verbose* 'nil) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ "Path of quicklisp? ")
(princ ";;; load quicklisp system" stream)
(princ #\newline stream)
(princ 
    `(load 
        ,(write-to-string (concatenate 'string 
            (read-line)
             "/setup.lisp")))
    stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; load cl-irc library" stream)
(princ #\newline stream)
(princ
    `(with-open-file 
        (*standard-output* ,(write-to-string "/dev/null")
            \:direction \:output
            \:if-exists \:supersede)
        (QL\:QUICKLOAD ,(write-to-string "cl-irc"))) 
    stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; define :robit package" stream)
(princ #\newline stream)
(prin1 '(defpackage :robit (:use :common-lisp :irc) (:export boot)) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; move scope to :robit package" stream)
(princ #\newline stream)
(prin1 '(in-package :robit) stream)
(princ #\newline stream)
(princ #\newline stream)

;(princ "Path of robit? ")
(princ ";;; set path directory for robit" stream)
(princ #\newline stream)
(princ `(defparameter *path* ,(write-to-string *robit-path*)) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ "Server of robit? ")
(princ ";;; set server of robit" stream)
(princ #\newline stream)
(princ `(defparameter *server* ,(write-to-string (read-line))) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ "Nick of robit? ")
(princ ";;; set nick of robit" stream)
(princ #\newline stream)
(princ `(defparameter *nick* ,(write-to-string (read-line))) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; set password of robit" stream)
(princ #\newline stream)
(princ "Is nick guarded? ")
(princ 
    `(defparameter *password* 
        ,(cond
            ((equal (read-line) "yes")
                (princ "Password of robit? ")
                (write-to-string (read-line)))
            (t 'nil)))
    stream)
(princ #\newline stream)
(princ #\newline stream)

;car, if atoms were words and lists were strings
(defun head (string)
    (let
        ((position (search " " string)))
        (cond
            ((eq position 'nil) string)
            (t (subseq string 0 position)))))

;cdr, if atoms were words and lists were strings
(defun tail (string)
    (let
        ((position (search " " string)))
        (cond
            ((eq position 'nil) "")
            (t (subseq string (+ 1 position) (length string))))))

;tokenize string into list of strings
(defun tokenize (string) 
    (cond 
        ((equal string "") 'nil)
        (t 
            (cons (write-to-string (head string)) (tokenize (tail string))))))

(princ "Channels of robit? ")
(princ ";;; set channels of robit" stream)
(princ #\newline stream)
(princ `(defparameter *channels* ,(cons 'list (tokenize (read-line)))) stream)
(princ #\newline stream)
(princ #\newline stream)

(princ ";;; set accessories of robit" stream)
(princ #\newline stream)
(princ "Auto-load accessory modules? ")
(princ 
    `(defparameter *accessories* 
        ,(cond
            ((equal (read-line) "yes")
                (princ "Accessories of robit? ")
                (cons 'list (tokenize (read-line))))
            (t 'nil)))
    stream)
(princ #\newline stream)

(close stream)
