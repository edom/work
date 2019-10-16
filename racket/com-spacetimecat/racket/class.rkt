#lang s-exp "lang.rkt"

(require
    (for-syntax
        racket/syntax
    )
)

(require+provide
    racket/class
)

(provide
    define-property
    define/forward
)

(begin-for-syntax
    (define-splicing-syntax-class after$
        #:datum-literals (->)
        [pattern (~seq #:after-change (old:id new:id -> body:expr ...))]
        [pattern (~seq)
            #:with old #'old
            #:with new #'new
            #:with (body ...) #'((void))
        ]
    ))

(define-syntax-parser define-property
    #:datum-literals (->)
    [   (_ $:id init:expr after:after$)
        (with-syntax (
                [backing (format-id #'$ "_backing_~a" #'$)]
                [get-$ (format-id #'$ "get-~a" #'$)]
                [set-$ (format-id #'$ "set-~a" #'$)]
                [before-$-change (format-id #'$ "before-~a-change" #'$)]
                [after-$-change (format-id #'$ "after-~a-change" #'$)]
            )
            #'(begin
                (define backing init)
                (define/public (get-$) backing)
                (define/public (set-$ new)
                    (define old backing)
                    (before-$-change old new)
                    (set! backing new)
                    (after-$-change old new))
                (define/public (before-$-change old new) (void))
                (define/public (after-$-change after.old after.new) after.body ...)
                (define-syntax $ (make-set!-transformer (lambda (stx)
                    (syntax-case stx (set!)
                        [(set! id new) #'(set-$ new)]
                        [id (identifier? #'id) #'(get-$)]
                ))))
            )
        )
    ]
)

(define-syntax-rule (define/forward (Method Param ...) Field)
    (define/public (Method Param ...) (send Field Method Param ...)))
