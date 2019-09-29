#lang racket

(provide (all-defined-out))

(require syntax/parse)
(require syntax/parse/define)
(require "ontology.rkt")
(provide (all-from-out "ontology.rkt"))

;;  --------------------    The "DEFINE" form.

;;  A (DEFINE ...) reduces to a Definition.

(struct Definition [id value] #:prefab)

(begin-for-syntax
    (require syntax/parse)
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

;;  Internal.
(define-syntax-parser DEFINE_OBJECT
    #:datum-literals (TYPE WITH)
    [(_ Id TYPE Type WITH [Key Val] ...)
        #'(Definition 'Id (NEW Type WITH [Key Val] ...))
    ]
)
(define-syntax-parser DEFINE
    #:datum-literals (ACTION OBJECT PROCEDURE SERVER STORAGE TABLE TYPE VALUE WITH)
    [(_ STORAGE Id (~seq Key Val) ...)
        #'(DEFINE_OBJECT Id TYPE Storage WITH [Key Val] ...)
    ]
    [(_ TABLE Id (~seq Key Val) ...)
        #'(DEFINE_OBJECT Id TYPE Table WITH [Key Val] ...)
    ]
    [(_ SERVER Id (~seq Key Val) ...)
        #'(DEFINE_OBJECT Id TYPE Server WITH [id Id] [Key Val] ...)
    ]
    [(_ VALUE Id Expr)
        #'(DEFINE_OBJECT Id TYPE Value WITH [value Expr])
    ]
    [(_ PROCEDURE Name
            (~alt
                Input:procedure_input%
                Output:procedure_output%
                (~once (ACTION Action ...))
            )
            ...
        )
        #'(DEFINE_OBJECT Name TYPE Procedure WITH
            [name Name]
            [inputs ((Input.Id Input.Type) ...)]
            [outputs (Output.Id ...)]
            [action #'(Action ...)]
        )
    ]
)
