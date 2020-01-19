(in-package #:stc-lisp)

(defun ensure-package (name)
  (declare (type string name))
  (or (find-package name) (make-package name)))

(defmacro with-gensyms (names &body body)
  (let ((bindings (loop for name in names collect (list name (gensym)))))
    `(let ,@bindings ,@body)))

(defmacro unconsing (econs names &body body)
  (let ((ncar (first names))
        (ncdr (second names))
        (nval (gensym)))
    `(let ((,nval ,econs))
      (let ((,ncar (car ,nval))
            (,ncdr (cdr ,nval)))
        ,@body))))

(defun gmap (func form)
  "Generalized MAP."
  (labels ((doit (form)
      (match form
        ((type symbol) (funcall func form))
        ((type number) (funcall func form))
        ((type string) (funcall func form))
        ((type vector) (map 'vector #'doit form))
        ((type cons) (cons (doit (car form)) (doit (cdr form))))
        (_ (error "Don't know how to deconstruct ~S" form)))))
    (doit form)))

(defun replace-package (&key ((:from srcpkg)) ((:to dstpkg)) ((:in form)))
  "Return a copy of form but with each symbol Srcpkg::Name replaced with Dstpkg::Name."
  (declare (type package from to))
  (gmap (lambda (form)
      (if (and (symbolp form) (eq (symbol-package form) srcpkg))
          (intern (symbol-name form) dstpkg)
          form))
    form))

(defpackage #:stc-lisp/dummy/unqualified
  (:documentation "an internal package for unqualified symbols")
  (:use #:stc-lisp))

(defconstant +dummy-package+ (find-package '#:stc-lisp/dummy/unqualified))

(defun read+ (&key ((:from stream)) ((:into dstpkg)))
  "Like READ but reads unqualified symbols into Dstpkg instead of into *PACKAGE*."
  (replace-package
    :from +dummy-package+
    :to dstpkg
    :in (let ((*package* +dummy-package+)) (read stream))))

;;;;    What.

(defvar *previous-package* nil)

(defmacro begin-package-neutral-reading ()
  `(progn
    (setf *previous-package* *package*)
    (in-package #:dummy-stc-reader)))

(defmacro end-package-neutral-reading ()
  `(setf *package* *previous-package*))
