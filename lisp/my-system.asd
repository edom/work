(defsystem "my-system"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :depends-on (
    (:version "asdf" "3.0.0")
    (:version "trivia" "0.1")
    "clack"
    "clack-handler-hunchentoot"
    "cl-dbi"
    "dbd-postgres"
  )
  :components (
    (:file "namespace")
    (:file "lisp0" :depends-on ("namespace"))
    (:file "asdf" :depends-on ("lisp0"))
    (:file "lisp1" :depends-on ("lisp0"))
    (:file "main" :depends-on ("lisp1" "asdf"))
  ))
