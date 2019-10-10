#lang s-exp "lang.rkt"

(require
    (for-syntax
        racket/list
    )
    racklog
)

(provide
    (rename-out
        [module-begin #%module-begin]
    )
    quote
    #%app
    #%datum
)

(module reader syntax/module-reader rackbol/private/prolog-like)

;;  U+2200  ∀   for-all
;;  U+2203  ∃   there-exists

(begin-for-syntax
    (define-splicing-syntax-class Forall%
        #:datum-literals (FOR ALL FORALL FOR-ALL ∀)
        (pattern (~seq ∀))
        (pattern (~seq FOR ALL))
        (pattern (~seq FORALL))
        (pattern (~seq FOR-ALL))
    )
    (define-splicing-syntax-class Exists%
        #:datum-literals (EXISTS THERE THERE-EXISTS ∃)
        (pattern (~seq ∃))
        (pattern (~seq EXISTS))
        (pattern (~seq THERE EXISTS))
        (pattern (~seq THERE-EXISTS))
    )
    (define-syntax-class Phrase$
        #:attributes (id [arg 1])
        (pattern [id arg:expr ...])
    )
    (define-syntax-class Clause$
        #:datum-literals (:-)
        #:attributes (id head [body 1] racklog)
        (pattern [head:Phrase$]
            #:attr (body 1) #f
            #:with id #'head.id
            #:with racklog #'((head.arg ...))
        )
        (pattern [head:Phrase$ :- body:Phrase$ ...]
            #:with id #'head.id
            #:with racklog #'((head.arg ...) body ...)
        )
    )
    (define-syntax-class Assertion$
        #:datum-literals (:-)
        #:attributes (id [var 1] clause racklog)
        (pattern [_:Forall% ~! [var:id ...] clause:Clause$]
            #:with id #'clause.id
            #:with racklog #'clause.racklog
        )
        ;;  Ground fact.
        (pattern clause:Phrase$
            #:with id #'clause.id
            #:with (var ...) #'()
            #:with racklog #'((clause.arg ...))
        )
    )
)

(define-for-syntax id-defined (make-hash))

(define-syntax (interpret stx)
    (syntax-parse stx
        [   (_ a:Assertion$)
            (let* (
                    [id (syntax-local-eval #''a.id)]
                )
                (with-syntax (
                        [Maybe-Define
                            (if (hash-ref id-defined id #f)
                                #'(void)
                                (begin
                                    (hash-set! id-defined id #t)
                                    #'(begin
                                        (define a.id %empty-rel)
                                        (provide a.id)
                                    )
                                )
                            )
                        ]
                    )
                    #'(begin
                        Maybe-Define
                        (%assert! a.id (a.var ...) a.racklog)
                    )
                )
            )
        ]
    )
)

(define-syntax (module-begin stx)
    (syntax-parse stx
        [   (_ Body ...)
            #'(#%module-begin (interpret Body) ...)
        ]))
