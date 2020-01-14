;;;;    User initialization file with *LOAD-TRUENAME*.

;;;;    Configure ASDF.

(require "asdf")
(asdf:version-satisfies (asdf:asdf-version) "3.0.0")

(asdf:ensure-source-registry
  '(:source-registry
     (:tree :here)
     :inherit-configuration))

(load "config.lisp")
(load *ql-setup-file*)
