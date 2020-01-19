(uiop:define-package #:stc-lisp
  (:documentation "COMMON-LISP and some customizations.")
  (:use-reexport #:common-lisp)
  (:export
    ;;; packages
    #:defpackage+
    #:define-package+
    ;;; lisp0
    #:fset
    #:fdefalias
    #:list-map
    #:match
    ;;; lisp1
    #:newline
    #:writeln
    #:println
    #:hash-ref
    ;;; file-local-package
    #:flp-defpackage
    #:flp-defpackage-empty
    #:flp-defpackage+enter
    #:flp-in-package
    #:flp-resolve
    ;;; read
    #:read+
    #:gmap
  ))

(in-package #:stc-lisp)

(defmacro defpackage+ (name &body body)
  "DEFPACKAGE and then IN-PACKAGE."
  `(progn
    (defpackage ,name ,@body)
    (in-package ,name)
  ))

(defmacro define-package+ (name &body body)
  "UIOP:DEFINE-PACKAGE and then IN-PACKAGE."
  `(progn
    (uiop:define-package ,name ,@body)
    (in-package ,name)
  ))
