#lang s-exp "../lang.rkt"

(provide (all-from-out "../lang.rkt"))

;;  Problem: 2019-10-13:
;;  I tried "framework", but it slows down startup.

(require+provide
    racket/gui/base
    "../core.rkt"
)
