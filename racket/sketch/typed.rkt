#lang racket/base

(require
    (for-syntax
        racket/base
        racket/list
        racket/syntax
        "type.rkt"
    )
    syntax/parse/define
)

(provide
    (except-out (all-from-out racket/base)
        #%datum
        #%app
        define
        lambda
    )
    (rename-out
        [_datum #%datum]
        [_app #%app]
        [_define define]
        [_lambda lambda]
    )
    :
    :type-of
)

(define-syntax-rule (define-syntax-case (Id Stx) Literals Case ...)
    (define-syntax (Id Stx)
        (syntax-case Stx Literals
            Case ...)))

;;  "(: Expression Type)" expands to "Expression" but with a "type" syntax-property.

(define-syntax-parser :
    [   (_ expr:Expr$ type_)
        #:attr type ($eval1 #'type_)
        #'expr
    ])

(define-syntax-parser :type-of
    [   (_ expr:Expr$)
        (datum->syntax #'expr (attribute expr.type))
    ])

(define-syntax-parser _datum
    [   (_ . Datum)
        #:attr type (datum-type (syntax->datum #'Datum))
        #'(#%datum . Datum)
    ])

(define-syntax-case (_app stx) ()
    [   (_ Func Arg)
        (with-syntax [
                (TFunc #f)
                (TArg #f)
            ]
            #'(#%app (: Func TFunc) (: Arg TArg))
        )
    ])

(define-syntax-case (_define stx) ()
    [   (_ Id Init)
        (with-syntax* [
                (TInit #'Any)
                (TId #'Any)
            ]
            #'(define TId TInit))
    ])

(define-syntax-case (_lambda stx) ()
    [   (_ (Param ...) Body ...)
        (with-syntax [
                ([TParam ...] #'(Param ...))
                ([TBody ...] #'(Body ...))
                (TLambda #f)
            ]
            #'(: (lambda (TParam ...) TBody ...) TLambda))
    ])
