(in-package #:common-lisp-user)

(defmacro defpackage+ (name &body body)
  "DEFPACKAGE and then IN-PACKAGE."
  `(progn
    (defpackage ,name ,@body)
    (in-package ,name)
  ))

(defpackage+ #:stc/lisp0
  (:use #:common-lisp)
  (:export #:list-map))

(defun list-map (func list)
  (map 'list func list))

(in-package #:common-lisp-user)
(use-package '#:stc/lisp0)
