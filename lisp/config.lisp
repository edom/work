;;;;    Quicklisp paths.

(defvar *ql-home-dir* (merge-pathnames
  (make-pathname :directory '(:relative "quicklisp"))
  (user-homedir-pathname)))

(defvar *ql-setup-file* (merge-pathnames "setup.lisp" *ql-home-dir*))
