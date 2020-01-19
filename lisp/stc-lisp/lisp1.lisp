(in-package #:stc-lisp)

;;;;    Scheme conventions.

;;;;    Transput.

(defun newline () (terpri))

;;  These names may be confusing.

(defmacro writeln (&rest args)
  `(progn (prin1 ,@args) (newline)))

(defmacro println (&rest args)
  `(progn (princ ,@args) (newline)))

;;;;    Hash tables.

(defun hash-ref (tab key)
  (gethash key tab))
