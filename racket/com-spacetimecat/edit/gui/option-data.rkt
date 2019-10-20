#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
)

(provide
    (struct-out Option)
    make-option
)

;;  This is about the data, not about the appearance.
;;  key is usually string.
;;  value is any user data.
;;  In semiotics, key is the syntax, and value is the semantics.

(struct Option (key value) #:prefab)

(define (make-option key value)
    (-> string? any/c Option?)
    (Option key value))
