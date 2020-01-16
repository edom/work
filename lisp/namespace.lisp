(uiop:define-package #:stc/namespace
  (:use #:common-lisp)
  (:export #:begin-gensym-package
           #:namespace
           #:package-used-by
           #:with-package))

(in-package #:stc/namespace)

(defvar *package-gensym-prefix* "#GENSYMPKG"
"GENSYM prefix for BEGIN-GENSYM-PACKAGE.")

(defvar *defpackage* 'uiop:define-package
"Either UIOP:DEFINE-PACKAGE or COMMON-LISP:DEFPACKAGE.")

(defun generate-package-name ()
  (gensym *package-gensym-prefix*))

(defmacro package-used-by (user)
  `(use-package *package* ,user))

(defmacro begin-gensym-package (&body body)
"Fake 'anonymous' packages.

UIOP:DEFINE-PACKAGE followed by CL:IN-PACKAGE."
  (let ((pkg (generate-package-name)))
    `(progn
      (,*defpackage* ,pkg ,@body)
      (in-package ,pkg)
      )))

(defmacro with-in-package (name &body body)
  (let ((tmp (gensym)))
    `(progn
      (let ((,tmp *package*))
        (unwind-protect
          (progn
            (in-package ,name)
            ,@body)
          (in-package ,tmp))))))

;;  What should (defvar ns (namespace ...)) compile to?
(defmacro namespace (&body body)
  (let ((name (generate-package-name)))
    `(progn
      (make-package ,name)
      (:use #:common-lisp)
      (with-in-package ,name ,@body))))

;;  (defnsvar Var TopLevelForm...)
;;  => (defvar Var Gensym) (make-package Gensym) (in-package Gensym) TopLevelForm... (in-package Saved)
