(defsystem "my-system"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :depends-on (
    (:version "asdf" "3.0.0")
    "trivia"
  )
  :components (
    (:file "lisp0")
    (:file "match" :depends-on ("lisp0"))
    (:file "asdf" :depends-on ("lisp0" "match"))
    (:file "lisp1" :depends-on ("lisp0" "match"))
    (:file "main" :depends-on ("lisp1" "asdf"))
  ))
