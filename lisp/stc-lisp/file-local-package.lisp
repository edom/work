(in-package #:stc-lisp)

;;;;    File-Local Packages.
;;;;    They are implemented by prepending file paths to ordinary Common Lisp package names.

(defun caller-pathname () *compile-file-pathname*)

(defun flp-compute-name (file name)
  "Translate a file-local package name to a Common Lisp package name.
FILE should be a truename.
NAME is a string."
  (declare (type (or pathname string) file)
           (type string name))
  (concatenate 'string "#flp:" (namestring file) ":" name))

;; 2020-01-17: SBCL 1.4.16: If "file" is nil, "merge-pathnames" corrupts memory?
(defun flp-resolve-f (caller ref
                 &aux file name truefile)
  "See FLP-RESOLVE."
  (setf (values file name)
    (cond
      ((stringp ref) (values (make-pathname) ref))
      ((consp ref) (values (first ref) (second ref)))
      (t (error "flp-resolve: Invalid reference: ~S" ref))))
  (setf truefile (truename (merge-pathnames file caller)))
  (flp-compute-name truefile name))

(defmacro flp-resolve (ref)
  "(flp-resolve NAME)
(flp-resolve (FILE NAME)) => PKGNAME
FILE is a string that is a relative path to a file that must exist.
FILE is resolved against (CALLER-PATHNAME) (where the flp-resolve form occurs).
NAME is file-local package name that is fed to FLP-DEFPACKAGE.
PKGNAME is a string."
  `(flp-resolve-f ,(caller-pathname) ,ref))

;; TODO: IMPORT/SHADOW instead of USE?
(defmacro flp-defpackage (name &body body)
  "Generate a file-local package.
NAME is a string literal (not a symbol). It is not evaluated.
BODY is passed to DEFPACKAGE."
  `(defpackage ,(flp-compute-name *compile-file-pathname* name)
    (:use #:common-lisp #:stc-lisp)
    ,@body))

(defmacro flp-defpackage+enter (name &body body)
  "FLP-DEFPACKAGE followed by FLP-IN-PACKAGE."
  `(progn
    (flp-defpackage ,name ,@body)
    (flp-in-package ,name)))

(defmacro flp-defpackage-empty (name &body params)
  "FLP-DEFPACKAGE without default :USE clauses."
  `(defpackage ,(flp-compute-name *compile-file-pathname* name) ,@params))

(defmacro flp-in-package (ref)
  "Similar to CL:IN-PACKAGE. The argument is not evaluated.
Example: (flp-in-package \"foo\")
Example: (flp-in-package (\"test.lisp\" \"foo\"))"
  `(cl:in-package ,(flp-resolve-f *compile-file-pathname* ref)))
