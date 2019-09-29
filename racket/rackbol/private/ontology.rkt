#lang racket

(provide (all-defined-out))

(require syntax/parse/define)

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
(begin-for-syntax
    (define-splicing-syntax-class NEW_Property
        [pattern (Key:identifier Val:expr)]
        [pattern Key:identifier #:with Val #'Key]
    )
)
(define-syntax-parser NEW
    #:datum-literals (WITH)
    [(_ Type WITH Prop:NEW_Property ...)
        #'(make_object_from_alist `(($type . Type) (Prop.Key . Prop.Val) ...))
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
