(load "config.lisp")

(defvar *ql-boot-file*
  (find-if #'probe-file '(
    "/usr/share/cl-quicklisp/quicklisp.lisp" ;; Debian 9
    "/usr/share/common-lisp/source/quicklisp/quicklisp.lisp" ;; Debian 10
  ))
  "Assume 'apt install cl-quicklisp' in Debian 9 or Debian 10.")

(load *ql-boot-file*)

(defun install-quicklisp ()
  (quicklisp-quickstart:install :path *ql-home-dir*))

(if (probe-file *ql-setup-file*)
  (format t "Quicklisp has already been installed.~%")
  (install-quicklisp))
