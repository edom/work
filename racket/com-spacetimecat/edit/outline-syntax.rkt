#lang s-exp "lang.rkt"

(require+provide
    syntax/parse
    "../racket/syntax.rkt"
)

(provide
    define$
    class-expr$
    define-class-form$
)

;;  Classes.

(define-syntax-rule (define-datum-literal-syntax-class Id Literal ...)
    (define-syntax-class Id
        #:datum-literals (Literal ...)
        [pattern (~or* Literal ...)]
    ))

(define-datum-literal-syntax-class define$
    define
    define/contract
    define/public
    define/pubment
    define/augment
    define/override
)

(define-syntax-class class-expr$
    #:datum-literals (class class*)
    [pattern (class super:expr body ...) #:with (iface ...) #'()]
    [pattern (class* super:expr (iface:expr ...) body ...)])

(define-syntax-class define-class-form$
    [pattern (_:define$ id:id class:class-expr$)
        #:with super #'class.super
        #:with (iface ...) #'(class.iface ...)
        #:with (body ...) #'(class.body ...)
    ])
