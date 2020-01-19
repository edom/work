(defsystem "stc-lisp"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :description "My custom Common Lisp"
  :depends-on (
    (:version "asdf" "3.0.0")
    (:version "trivia" "0.1")
  )
  :components (
    (:file "packages")
    (:file "file-local-package" :depends-on ("packages"))
    (:file "lisp0" :depends-on ("packages" "file-local-package"))
    (:file "read" :depends-on ("packages" "lisp0"))
    (:file "lisp1" :depends-on ("packages" "lisp0"))
  ))

(defsystem "stc-lisp/test"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :depends-on ("stc-lisp")
  :components ((:file "test")))
