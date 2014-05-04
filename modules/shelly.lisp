;;;; shelly.lisp

(defun stream-to-list (a-stream)
    (let
        ((line
            (read-line a-stream nil)))
        (cond
            ((eq line 'nil)
                '())
            (t
                (cons line (stream-to-list a-stream))))))

(defun $ (command)
    (stream-to-list
        (run-shell-command command :output :stream)))
