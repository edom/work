#lang racket

(provide (all-defined-out))

(require syntax/parse)
(require syntax/parse/define)
(require (for-syntax syntax/parse))

;;  --------------------    Ontology with class-property-instance.
;;  This is the input of the code generator.
;;  We interpret the user's model to mutate this state.

;;  https://stackoverflow.com/questions/38130826/can-i-instantiate-a-module-multiple-times-in-one-racket-program

(define make_object_from_alist make-hash)
(define get_object_property hash-ref)
(define set_object_property hash-set!)

(define-syntax-parser CLASS
    #:datum-literals (PROPERTY)
    [(_ Name [~alt (PROPERTY Prop_Name)] ...)
        #'(list 'Name '(Prop_Name ...))
    ]
)
(define-syntax-parser NEW
    #:datum-literals (WITH)
    [(_ Type WITH [Key Val] ...)
        #'(make_object_from_alist `(($type . Type) (Key . Val) ...))
    ]
)
(define-syntax-parser TYPE
    #:datum-literals (OF)
    [(_ OF Instance)
        #'(GET $type OF Instance)
    ]
)
(define-syntax-parser GET
    #:datum-literals (OF)
    [(_ Property OF Instance)
        #'(get_object_property Instance `Property)
    ]
)
(define-syntax-parser SET
    #:datum-literals (OF TO)
    [(_ Property OF Instance TO Replacement)
        #'(set_object_property Instance `Property Replacement)
    ]
)

;;  --------------------    The "DEFINE" form.

;;  A (DEFINE ...) reduces to a Definition.

(struct Definition [id value] #:prefab)

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

;;  Internal.
(define-syntax-parser DEFINE_OBJECT
    #:datum-literals (TYPE WITH)
    [(_ Id TYPE Type WITH [Key Val] ...)
        #'(Definition 'Id (NEW Type WITH [Key Val] ...))
    ]
)
(define-syntax-parser DEFINE
    #:datum-literals (ACTION OBJECT PROCEDURE STORAGE TABLE TYPE VALUE WITH)
    [(_ STORAGE Id (~seq Key Val) ...)
        #'(DEFINE_OBJECT Id TYPE Storage WITH [Key Val] ...)
    ]
    [(_ TABLE Id (~seq Key Val) ...)
        #'(DEFINE_OBJECT Id TYPE Table WITH [Key Val] ...)
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
