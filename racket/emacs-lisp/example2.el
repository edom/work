;;  eval defun
(eval '(defun f (x) (+ x 3)))
(print (f 1))

;;  redefinition
(defun f (x) (+ x 1))
(print (f 1))
(defun f (x) (+ x 2))
(print (f 1))

;;  variable
(setq a 1)
(print a)

;;  Emacs has two separate namespaces: functions and variables.
(funcall '+ 1 2)
(print (progn 1))

(defun f (x) (+ x 1))
(fset 'g 'f)
(fset 'h 'g)
(print (h 200))
(print (symbolp (symbol-function 'h)))

(defun addx (y) (+ x y))

;;  dynamic binding
(let ((x 1))
    (print x)           ; 1
    (print (addx 2))    ; 3
    (print x)           ; 1
)

;;  macro
(defmacro plus (x y) (+ x y))
(print (plus 10 20))
