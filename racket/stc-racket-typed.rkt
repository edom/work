#lang racket/base

(require
    "stc-racket.rkt"
    (for-syntax "type.rkt")
)

(provide
    (except-out (all-from-out racket/base)
        ;#%app
        #%datum
    )
    (rename-out
        ;(app #%app)
        (datum #%datum)
    )
)

#|
(define-syntax (app stx)
    (syntax-case stx ()
        ((_ a ...)
            #'(#%app a ...)
        )
    )
)
|#

(define (: expr type)
    expr
)

(define-syntax (datum stx)
    (syntax-case stx ()
        ((_ . s)
            (let ((dat (syntax->datum #'s)))
                #`(: #,dat #,(apparent-type-of dat))
            )
        )
    )
)
