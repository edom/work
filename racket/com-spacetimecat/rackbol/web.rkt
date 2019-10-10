#lang s-exp "base.rkt"

(provide (all-from-out "base.rkt"))

(require+provide
    "model.rkt"
    "for.rkt"
    "generate.rkt"
    "sql-syntax.rkt"

    db
    web-server/servlet
    web-server/servlet-env
)

;;  Translation:
;;
;;  Each storage translates to a virtual connection of the db library.
