(declaim (optimize (speed 0) (debug 2)))

(defpackage #:stc-test
  (:use #:stc-lisp))

(in-package #:stc-test)

(defun qualified-name (sym)
  (declare (type symbol sym))
  (let* ((name (symbol-name sym))
         (pkg (symbol-package sym))
         (pkgname (if pkg (package-name pkg) ""))
         (sep (if pkg "::" "")))
    (concatenate 'string pkgname sep name)))

(defun printsymq (sym)
  (declare (type symbol sym))
  (princ (qualified-name sym)))

(let ((*print-escape* t))
  (terpri)
  (printsymq 'a) (terpri)
  (printsymq '#:foo) (terpri)
  (printsymq 'asdf::install) (terpri)
  (printsymq ':what) (terpri))

(make-package '#:dumpak)
(with-open-stream (s (make-string-input-stream "(0 \"s\" (a b (c d)) #(1 a (b)) cl:append)"))
  (let ((*print-escape* t))
    (pprint (read+ :from s :into (find-package '#:dumpak))))
  (terpri))

(cl:defpackage #:dummy)
(cl:in-package #:dummy)
;(defun dummy::f () 0) ;; should fail
;(cl:defun dummy::f () 0) ;; should work

(cl:defpackage #:dummy0)
(cl:in-package #:dummy0)
(stc-lisp:println (cl:symbol-package 'a))

(cl:defpackage #:dummy1)
(cl:in-package #:dummy1)
(stc-lisp:println (cl:symbol-package 'a))

(cl:in-package #:stc-test)

(flp-defpackage "foo")
(flp-defpackage "bar")
(flp-in-package "foo") (println *package*)
(flp-in-package "bar") (println *package*)
(flp-in-package ("test.lisp" "foo")) (println *package*)
(println (flp-resolve '("test.lisp" "foo")))

#|
;;; Some ideas about syntax.

(glet* var Name = Form
       fun Name (Param*) = Form
       mac Name (Param*) = Form
   in Form*)

;;; I strongly feel that there has to be an existing implementation
;;; of this pattern-matching language somewhere in the Internet.

(mmatch form
  ((var $name = $form)
    `(let ((,$name ,$form)) ,@body)))
|#

;;;;    Surface-syntax substitution.

(eval-when (:compile-toplevel)
  (defun fbindings->table (fbindings)
    (let ((table (make-hash-table)))
      (loop for fbinding in fbindings
            for symbol = (first fbinding)
            and defin = (second fbinding)
            do (setf (gethash symbol table) defin))
      table))
  (defun my-replace (table form)
    (gmap (lambda (form)
        (if (symbolp form)
          (gethash form table form)
          form))
      form)))

(defmacro slet (fbindings &body body)
  "Naive surface-syntax substitution from outside inwards."
  (let ((table (fbindings->table fbindings)))
    `(progn ,@(my-replace table body))))

(let ((*print-escape* nil))
  ;;  SYMBOL-MACROLET does not trespass QUOTE.
  ;;  SYMBOL-MACROLET evaluates bindings.
  (pprint (symbol-macrolet ((x 'dummy0::x) (y 'dummy1::y)) (list 'progn x 'y 'dummy0::z)))

  ;;  SLET trespasses QUOTE.
  ;;  SLET does not evaluate bindings.
  (pprint (slet ((x dummy0::x) (y dummy1::y)) '(a x y dummy0::z)))

  ;;  SLET is expanded from outside inwards.
  (pprint (slet ((x 1)) (slet ((y x)) '(x y))))

  ;;  SLET bindings must not shadow. This is undefined.
  (pprint (slet ((x 1)) (slet ((x 2)) '(x y))))

  (pprint (slet ((dummy0::x dummy1::y)) '(x dummy0::x y dummy1::y)))
)

#|
(file-local-package Name
  Directive*
  Body*)

Directive:
  (:translate Sym Def) ;; replace all occurrence of Sym in Body with Def
  (:import-all-from Package)

Features:
  - Import all from a package.
  - Rename imports (prefix? arbitrary function mapping?).
|#
