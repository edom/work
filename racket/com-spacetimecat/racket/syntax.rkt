#lang racket/base

(require
    (for-syntax
        racket/base
        syntax/parse
    )
    "require-provide.rkt"
)

(require+provide
    (for-syntax
        racket/syntax
        "syntax-parse.rkt"
    )
    syntax/parse/define
    syntax/srcloc
    "syntax-parse.rkt"
)

(provide

    identifier->string
    string->identifier

    $eval1

    define-syntax-case
    define-syntax-rules

    $if

    syntax-srcloc
    syntax-position-1   ;;  one-based index
    syntax-position-0   ;;  zero-based index

    stx-append-map

)

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

(begin-for-syntax
    (define-syntax-class Define-Syntax-Head$
        [pattern id:id
            #:with stx #'stx
            #:with head #'(id stx)
        ]
        [pattern (id:id stx:id)
            #:with head #'(id stx)
        ]
    )
)

;;  This is a common pattern that combines define-syntax and syntax-case.
;;
;;  (define-syntax-case Head
;;      Opt-Literals
;;      Case ...)
;;  =>
;;  (define-syntax (Id stx)
;;      (syntax-case stx (literal-id ...)
;;          Case ...
;;      )
;;  )
;;
;;
;;  Head            ::= id | (id stx-id)
;;
;;
;;  Opt-Literals    ::=
;;                  |   #:literals (literal-id ...)
;;
;;  Case is a case as in syntax-case.

(define-syntax-parser define-syntax-case
    [   (_
            Head:Define-Syntax-Head$
            [~optional-seq #:literals (Literal ...)]
            Case ...
        )
        #:with Literals #'(~? (Literal ...) ())
        #'(define-syntax (Head.id Head.stx)
            (syntax-case Head.stx Literals
                Case ...
            )
        )
    ]
)

(define-syntax-rule (define-syntax-rules Name Literals Clause ...)
    (define-syntax Name
        (syntax-rules Literals Clause ...)
    )
)

;;  Conditional expansion.

(define-syntax-case $if
    [   (_ Cond True False)
        (if (syntax-local-eval #'Cond)
            #'True
            #'False
        )
    ]
)

(define (syntax-srcloc stx)
    (make-srcloc
        (syntax-source stx)
        (syntax-line stx)
        (syntax-column stx)
        (syntax-position stx)
        (syntax-span stx)))

(define (syntax-position-1 s) (syntax-position s))
(define (syntax-position-0 s)
    (define pos1 (syntax-position-1 s))
    (if pos1 (- pos1 1) #f))

(define (stx-append-map f stx)
    (define (loop stx)
        (syntax-case stx ()
            [() #'()]
            [(head . tail) #`(#,@(f #'head) . #,(loop #'tail))]))
    (loop stx))
