(defpackage+ #:stc/asdf
  (:documentation "some ASDF enhancements")
  (:use #:stc-lisp)
  (:export #:print-dependency-tree))

;;  Debian 9 comes with ASDF 3.1.7.
;;  Debian 10 comes with ASDF 3.3.2.
;;  ASDF:SYSTEM-NAME and ASDF:SYSTEM-VERSION
;;  are in ASDF 3.3.2 but not in ASDF 3.1.7.

(defun system-name (system)
  (slot-value system 'asdf/system::name))

(defun system-version (system)
"This is supposed to work in ASDF 3.3?"
  (slot-value system 'asdf/system::version))

(defun asdf-dep-name (d)
"We don't know whether ASDF already provides a function like this."
  (match d
    ((list :version name _) (asdf-dep-name name))
    ((list :feature _ name) (asdf-dep-name name))
    ((list :require name) (asdf-dep-name name)) ;; ???
    ((type symbol) d)
    ((type string) d)))

(defstruct item
"Represents an ASDF system.
DEPENDS-ON is a list of ITEM."
  name version license depends-on)

(defun compute-dependency-tree (name)
"This takes a string and returns an ITEM."
  (let* ((system (asdf:find-system name))
         (deps (asdf:system-depends-on system)))
    (make-item
      :name name
      :version (system-version system)
      :license (asdf:system-licence system)
      :depends-on (list-map (lambda (dep) (compute-dependency-tree (asdf-dep-name dep))) deps))))

(defun pad (n)
  (loop repeat n do (princ " ")))

(defun pretty-print (item)
  (labels (
    (doit (level item)
      (match item
        ((item name version license depends-on)
          (pad level)
          (format t "~a ~a ~a~%" name version license)
          (loop for child in depends-on do
            (doit (+ level 1) child))))))
    (doit 0 item)))

(defun print-dependency-tree (name)
"Print the dependency tree of an ASDF system."
  (pretty-print (compute-dependency-tree name)))
