#lang racket/base

(require
    (for-syntax
        racket/base
    )
    "require-provide.rkt"
)

(require+provide
    syntax/parse
)

(provide
    ~permute
    ~permute-seq
    ~optional-seq
    ~and-not
)

;;  This is an EH-pattern (ellipsis-head pattern).
;;  Do not forget the trailing ellipsis after the last closing parenthesis.
;;
;;  For example:
;;
;;      (~permute A B C) ...
;;
;;  translates to:
;;
;;      (~alt
;;          (~once A)
;;          (~once B)
;;          (~once C)
;;      ) ...

(define-syntax ~permute
    (pattern-expander (lambda (stx)
        (syntax-case stx ()
            [(_ Thing ...)
                #'(~alt (~once Thing) ...)
            ]))))

;;  For example:
;;
;;      (~permute-seq
;;          (A0 A1 A2)
;;          (B0 B1 B2)
;;          (C0 C1 C2)
;;      ) ...
;;
;;  translates to:
;;
;;      (~alt
;;          (~once (~seq A0 A1 A2))
;;          (~once (~seq B0 B1 B2))
;;          (~once (~seq C0 C1 C2))
;;      ) ...

(define-syntax ~permute-seq
    (pattern-expander (lambda (stx)
        (syntax-case stx ()
            [(_ (Thing ...) ...)
                #'(~permute (~seq Thing ...) ...)
            ]))))

;;  (~optional-seq A ...) = (~optional (~seq A ...))

(define-syntax ~optional-seq
    (pattern-expander (lambda (stx)
        (syntax-case stx ()
            [(_ A ...)
                #'(~optional (~seq A ...))
            ]))))

;;  (~and-not A B) = (~and A (~not B))

(define-syntax ~and-not
    (pattern-expander (lambda (stx)
        (syntax-case stx ()
            [(_ A B)
                #'(~and A (~not B))
            ]))))
