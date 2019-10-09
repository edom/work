#lang s-exp "base.rkt"

(provide
    (all-from-out "base.rkt")
    define-constant
    DEFINE
)

;;  A (DEFINE ...) should produce an object.
;;  That is why we have define-constant and not DEFINE CONSTANT.

(define-syntax (define-constant stx)
    (syntax-case stx ()
        [(_ Id Expr)
            (with-syntax ([Internal (format-id #f "_~a" #'Id)])
                #'(begin
                    (define Internal Expr)
                    (define-syntax Id (make-set!-transformer
                        (lambda (stx)
                            (syntax-case stx (set!)
                                [(set! _ _) (raise-syntax-error #f "Cannot change a constant" stx)]
                                [id (identifier? #'id) #'Internal]
                            ))))))
        ]))

;;  --------------------    The "DEFINE" form.

(begin-for-syntax
    (define-splicing-syntax-class procedure_input%
        #:datum-literals (DEFAULT INPUT NAME TYPE)
        (pattern (INPUT Id
            (~optional (~seq NAME Name) #:defaults ((Name #''Id)))
            TYPE Type
            (~optional (~seq DEFAULT Default) #:defaults ((Default #f)))
        ))
    )
    (define-splicing-syntax-class procedure_output%
        #:datum-literals (NAME OUTPUT TYPE)
        (pattern (OUTPUT Id
            (~optional (~seq NAME Name) #:defaults ((Name #''Id)))
            TYPE Type
        ))
    )
)

(define-syntax-parser define-storage
    #:datum-literals (CATALOG HOST PASSWORD PORT postgresql TYPE USER)
    [   (_ Id:id Stor:Storage-Params$)
        #'(_DEFINE_OBJECT Id TYPE Storage WITH
            [type postgresql]
            [host Stor.host]
            [port Stor.port]
            [catalog Stor.catalog]
            [user Stor.user]
            [password Stor.password]
        )
    ])

(define-syntax-parser DEFINE
    #:datum-literals (ACTION OBJECT PROCEDURE SERVER STORAGE TABLE TYPE VARIABLE WITH)

    [(_ STORAGE Arg ...) #'(define-storage Arg ...)]

    [(_ TABLE Id (~seq Key Val) ...)
        #'(_DEFINE_OBJECT Id TYPE Table WITH [Key Val] ...)
    ]
    [(_ SERVER Id (~seq Key Val) ...)
        #'(_DEFINE_OBJECT Id TYPE Server WITH [Key Val] ...)
    ]
    [(_ VARIABLE Id Expr)
        #'(_DEFINE_OBJECT Id TYPE Value WITH [value Expr])
    ]
    [(_ PROCEDURE Name
            (~alt
                Input:procedure_input%
                Output:procedure_output%
                (~once (ACTION Action ...))
            )
            ...
        )
        #'(_DEFINE_OBJECT Name TYPE Procedure WITH
            [name Name]
            [inputs ((Input.Id Input.Type) ...)]
            [outputs (Output.Id ...)]
            [action #'(Action ...)]
        )
    ]
)
