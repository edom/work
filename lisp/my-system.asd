(defsystem "my-system"
  :version "0.0.0"
  :author "Erik Dominikus <software@spacetimecat.com>"
  :depends-on (
    (:version :asdf "3.0.0")
    :trivia
  )
  :components (
    (:file "scheme")
    (:file "main" :depends-on ("scheme"))
  ))
