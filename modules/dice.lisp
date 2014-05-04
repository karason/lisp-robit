;;;; dice.lisp

(defparameter ⚀ '1)
(defparameter ⚁ '2)
(defparameter ⚂ '3)
(defparameter ⚃ '4)
(defparameter ⚄ '5)
(defparameter ⚅ '6)

(defun dieify (numeric)
    (cond
        ((eq numeric '1) '⚀)
        ((eq numeric '2) '⚁)
        ((eq numeric '3) '⚂)
        ((eq numeric '4) '⚃)
        ((eq numeric '5) '⚄)
        ((eq numeric '6) '⚅)))

(defun roll-die ()
    (dieify (+ 1 (random 6))))

(defun roll-dice (quantity)
    (cond
        ((eq quantity '0) '())
        (t (cons (roll-die) (roll-dice (- quantity 1))))))

(defun compare-die (die-1 die-2)
    (let 
        ((numeric-1 (eval die-1))
         (numeric-2 (eval die-2)))
        (cond
            ((> numeric-1 numeric-2) '1)
            ((< numeric-1 numeric-2) '-1)
            ((= numeric-1 numeric-2) '0))))

(defun sort-dice (dice)
    (mapcar 'dieify (sort (mapcar 'eval dice) '>)))

(defun compare-dice (dice-1 dice-2)
    (let 
        ((score 
           (reduce '+ (mapcar 'compare-die (sort-dice dice-1) (sort-dice dice-2)))))
        (cond
            ((> score '0) '1)
            ((< score '0) '-1)
            ((= score '0) '0))))
