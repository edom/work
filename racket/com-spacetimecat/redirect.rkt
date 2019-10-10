#lang racket/base

(module reader syntax/module-reader com-spacetimecat/redirect)

(require
    (for-syntax
        racket/base
        racket/path
        racket/syntax
        syntax/parse
    )
    syntax/parse/define
)

(provide
    (except-out (all-from-out racket/base)
        #%module-begin
    )
    (rename-out
        (module-begin #%module-begin)
    )
)

(begin-for-syntax
    (require
        (for-syntax
            racket/base
        )
        syntax/parse/define
    )
)

(define-syntax (module-begin stx)
    (define this-module-path (syntax-source #'module-begin))
    (define expanding-module-path (resolved-module-path-name (current-module-declare-name)))
    (define/with-syntax (require-spec body ...)
        (syntax-parse stx
            [   (_ #:to require-spec body ...)
                #'(require-spec body ...)
            ]
            [   (_ #:to-my relative:string body ...)
                (let* (
                        [relative-path (syntax->datum #'relative)]
                        [absolute-path (path->string
                            (simplify-path
                                (build-path this-module-path ".." relative-path)
                                #f
                            )
                        )]
                    )
                    #`((file #,absolute-path) body ...)
                )
            ]
        ))
    #`(#%module-begin
        (require require-spec)
        (provide (all-from-out require-spec))
        (module reader syntax/module-reader #,expanding-module-path)
        body ...
    )
)
