;;;;    User initialization file.

;;  *LOAD-TRUENAME* is NIL because SBCL does READ-and-EVAL ~/.sblrc and does not LOAD it.
;;  That breaks ASDF's :HERE.

;;  Configure how LOAD resolves relative paths.
(let* ((this-file (truename (sb-impl::userinit-pathname)))
       (this-dir (make-pathname :directory (pathname-directory this-file))))
  (setf *default-pathname-defaults* this-dir))

(load "init.lisp")
