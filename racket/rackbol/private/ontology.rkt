#lang racket

(provide
    CLASS
    NEW
    GET
)

(require syntax/parse)
(require syntax/parse/define)

(define make_object_from_alist make-hash)
(define get_object_property hash-ref)

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
(define-syntax-parser GET
    #:datum-literals (OF)
    [(_ Property OF Instance)
        #'(get_object_property Instance `Property)
    ]
)
