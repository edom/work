;;;;    Example.

(defpackage #:whatisit
  (:use #:stc/lisp1))
(in-package #:whatisit)

(newline) ;; because ASDF sometimes clobber the output.

(stc/asdf:print-dependency-tree "my-system")

#|
;;  https://github.com/fukamachi/clack
(defvar *handler*
  (clack:clackup
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello, Clack!")))))
|#

(defvar foo 123)
(format t "-------------- ~a~%" *package*)
(format t "-------------- ~a~%" foo)

(println (gensym))
;;  Does not work.
;(defvar ns1 (namespace))
;(defvar ns2 (namespace))

(defmacro inpkg (name &body body)
  `(progn
    (cl:defpackage ,name)
    (cl:in-package ,name)
    ,@body
  ))

(cl:defpackage #:dummy0)
(cl:in-package #:dummy0)
(stc/lisp1:println (cl:symbol-package 'a))

(cl:defpackage #:dummy1)
(cl:in-package #:dummy1)
(stc/lisp1:println (cl:symbol-package 'a))

(whatisit::inpkg #:dummy2 (stc/lisp1:println (cl:symbol-package 'a)))
(whatisit::inpkg #:dummy3 (stc/lisp1:println (cl:symbol-package 'a)))

;;  We can defun something without in-package.
(defun dummy2::f () 1)
