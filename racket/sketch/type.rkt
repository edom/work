#lang racket/base

(require
    racket/base
    racket/list
    syntax/parse
)

(provide
    (struct-out TYPE)
    (struct-out TYPED)
    Expr$
    $eval1
    datum-type
)

(struct TYPE (name) #:prefab)

(struct TYPED (type value) #:prefab)

(define-syntax-rule (define-simple-type Id)
    (define Id (TYPE 'Id))
)

(define-simple-type Any)
(define-simple-type Boolean)
(define-simple-type Integer)
(define-simple-type Number)
(define-simple-type String)
(define-simple-type Symbol)
(define-simple-type Vector)
(define-simple-type Cons)
(define-simple-type Hash)
(define-simple-type Bytes)

(define-syntax-class Expr$
    #:attributes (type)
    (pattern exp:boolean #:attr type Boolean)
    (pattern exp:number #:attr type Number)
    (pattern exp:string #:attr type String)
    (pattern exp:id #:attr type Any)
    (pattern exp:expr #:attr type Any)
)

(define (datum-type datum)
    (cond
        [(number? datum) Number]
        [(string? datum) String]
        [(symbol? datum) Symbol]
        [(vector? datum) Vector]
        [(cons? datum) Cons]
        [(hash? datum) Hash]
        [(bytes? datum) Bytes]
        [else Any]
    ))

;;  FIXME: This is a duplicate of another $eval1.

(define ($eval1 stx)
    (syntax-case stx ()
        [(_ Exp)
            #'(let-syntax (
                    [(bring-down (lambda (stx) (datum->syntax stx Exp)))]
                )
                (bring-down)
            )
        ]
    ))
