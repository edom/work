(defsystem "my-system"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :depends-on (
    "stc-lisp"
    "clack"
    "clack-handler-hunchentoot"
    "cl-dbi"
    "dbd-postgres"
  )
  :components (
    (:file "asdf")
    (:file "main" :depends-on ("asdf"))
  ))
