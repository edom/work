#lang racket/base

;;  --------------------    syntax/parse enhancements.

(require
    (for-syntax
        racket/base
    )
    racket/base
    syntax/parse
)

(provide

    identifier->string
    string->identifier

    $eval1

    ~permute
    ~permute-seq
    ~optional-seq
    ~and-not

)

;;  --------------------

(define (identifier->string id)
    (symbol->string (syntax->datum id))
)

(define (string->identifier ctx str)
    (datum->syntax ctx (string->symbol str))
)

;;  The form ($eval1 Exp) expands to a datum syntax object that wraps Val,
;;  where Val is the result of evaluating Exp at phase level 1 at expansion time.
;;
;;  Example:
;;
;;      (define-for-syntax x (+ 1 2))
;;
;;      ;;  This produces 3.
;;      ($eval1 x)
;;
;;  If you need only Val without the syntax object wrapping,
;;  you can use syntax-local-eval instead.
;;
;;  Beware of Racket's Separate Compilation Guarantee.
;;  The expression evaluation should be free of mutation,
;;  and the returned value should not be mutated.
;;  The following is an unrealistic example of bad (legal but confusing) code:
;;
;;      (define-for-syntax port (open-output-string))
;;
;;      ;;  displayln inside the eval.
;;      ($eval1 (displayln "foo" port))
;;
;;      ;;  displayln outside the eval.
;;      (displayln "bar" ($eval1 port))
;;
;;      ;;  This produces "foo".
;;      (displayln ($eval1 (get-output-string port)))
;;
;;      ;;  This produces "foobar".
;;      (displayln (get-output-string ($eval1 port)))

(define-syntax ($eval1 stx)
    (syntax-case stx ()
        [(_ Exp)
            ;;  This let-syntax form could be replaced with the shorter expression
            ;;  (datum->syntax stx (syntax-local-eval #'Exp)).
            ;;  However, there is no need for eval here,
            ;;  because Racket is going to evaluate Exp anyway.
            #'(let-syntax
                ;;  bring-down is bound in phase 0,
                ;;  but the lambda is evaluated in phase 1.
                ([bring-down (lambda (stx) (datum->syntax stx Exp))])
                (bring-down)
            )
        ]))

;;  --------------------

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
