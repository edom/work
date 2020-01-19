;;;;    Example.

(defpackage #:whatisit
  (:use #:stc-lisp))

(in-package #:whatisit)

(newline) ;; because ASDF sometimes clobber the output.

(stc/asdf:print-dependency-tree "my-system")

;;  https://github.com/fukamachi/clack
(defvar *handler*
  (clack:clackup
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, Clack!")))))
