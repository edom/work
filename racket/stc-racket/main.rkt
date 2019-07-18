#lang racket/base

;;  stc-racket is racket with some renamings for consistency.
;;
;;  map is renamed to list-map.
;;
;;  struct is renamed to define-struct-2.
;;
;;  module is renamed to define-module.

(module reader syntax/module-reader stc-racket)

(define-syntax-rule (pass-through module ...)
    (begin
        (require module ...)
        (provide (all-from-out module) ...)
    )
)

;;;;    --------------------    imports

(pass-through
    "private/base.rkt"
    "private/container.rkt"
    "private/functional.rkt"
    "private/logic.rkt"
    "private/module.rkt"
    "private/object.rkt"
    "private/syntax.rkt"
    "private/transput.rkt"
)

;;;;    --------------------    exports

(provide
    #%app
    #%datum
    #%module-begin
    #%top
    #%top-interaction
)

;;;;    --------------------    operating system

;;  system* is a simple interface,
;;  for synchronously spawning subprocesses,
;;  that integrates well with ports.

(require racket/system)
(provide
    system*
)

(provide
    (rename-out
        (for-each list-for-each)
    )
)
