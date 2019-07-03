;;  stc-racket is racket with some renamings for consistency.
;;
;;  map is renamed to list-map.
;;
;;  struct is renamed to define-struct.
;;
;;  module is renamed to define-module.

(module stc-racket racket/base

    (define-syntax-rule (pass-through module ...)
        (begin
            (require module ...)
            (provide (all-from-out module) ...)
        )
    )

;;;;    --------------------    imports

    (pass-through
        "stc-racket/base.rkt"
        "stc-racket/container.rkt"
        "stc-racket/functional.rkt"
        "stc-racket/logic.rkt"
        "stc-racket/module.rkt"
        "stc-racket/object.rkt"
        "stc-racket/transput.rkt"
    )

;;;;    --------------------    exports

    (provide

        (rename-out
            (app #%app)
        )

        #%datum
        #%module-begin
        #%top

    )

;;;;    --------------------    customizations

    ;;  Problem: This explodes the syntax tree and may slow down the interpreter.
    ;;  Problem: How often is this feature used?

    ;;  https://clojure.org/guides/learn/functions
    ;;  Should we adopt Clojure's syntax for calling Java methods to call Racket class instance methods?

    (define-syntax-rule (app fun arg ...)
        (cond
            ((procedure? fun)
                (fun arg ...)
            )
            ((hash? fun)
                (hash-ref fun arg ...)
            )
            ((vector? fun)
                (vector-ref fun arg ...)
            )
            (else
                (error "cannot apply" fun arg ...)
            )
        )
    )
)
