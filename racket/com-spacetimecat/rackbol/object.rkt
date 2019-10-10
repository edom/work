#lang s-exp "lang.rkt"

;;  --------------------    Things defined here.

(provide

    ;;  Interface.

    NEW
    GET
    SET
    _DEFINE_OBJECT

    ;;  Implementation.

    get-object-property

)

;;  --------------------    Ontology with class-property-instance.

;;  --------------------    Interface.

(define-syntax-parser CLASS
    #:datum-literals (PROPERTY)
    [(_ Name [~alt (PROPERTY Prop_Name)] ...)
        #'(list 'Name '(Prop_Name ...))
    ]
)

(begin-for-syntax
    (define-splicing-syntax-class NEW_Property
        [pattern (Key:identifier Val:expr)]
        [pattern Key:identifier #:with Val #'Key]
    )
)

(define-syntax-parser NEW
    #:datum-literals (WITH)
    [(_ Type WITH Prop:NEW_Property ...)
        #'(make-object-from-alist `(($type . Type) (Prop.Key . Prop.Val) ...))
    ]
)

(define-syntax-parser TYPE
    #:datum-literals (OF OR)
    [(_ OF Instance)
        #'(GET $type OF Instance)
    ]
)

(define-syntax-parser GET
    #:datum-literals (OF OR)
    [(_ Property OF Instance)
        #'(get-object-property Instance `Property)]
    [(_ Property OF Instance OR Default ...)
        #'(get-object-property Instance `Property (lambda () Default ...))]
)

(define-syntax-parser SET
    #:datum-literals (OF TO)
    [(_ Property OF Instance TO Replacement)
        #'(set-object-property! Instance `Property Replacement)
    ]
)

(define-syntax-parser _DEFINE_OBJECT
    #:datum-literals (TYPE WITH)
    [(_ Id TYPE Type WITH [Key Val] ...)
        #'(begin
            (define Id (NEW Type WITH [id Id] [Key Val] ...))
        )
    ])

;;  --------------------    Implementation.

(define make-object-from-alist make-hash)
(define get-object-property hash-ref)
(define set-object-property! hash-set!)
