#lang s-exp "lang.rkt"

(require+provide/all
    racklog
)

(provide
    %for-each
    %FOR-EACH
    %exists
    %EXISTS
)

;;  This wraps %which and %more.
;;
;;  NOTE: Limitations:
;;  %FOR cannot be nested.
;;  Do not call %which in Body.
;;  Do not call %more in Body.
;;
;;  XXX: Hyrum's law in action:
;;  We depend on an undocumented behavior:
;;  We assume that %which returns the bindings in the same order we give the variables.

(define-syntax-parser %for-each
    [   (_ Var:id ... #:satisfying Goal:expr ...
            (~alt
                (~optional (~seq #:to-break-call Break:id) #:defaults [(Break #'_break)])
                (~optional (~seq #:to-skip-call Skip:id) #:defaults [(Skip #'_skip)])
            ) ...
            #:do Body ...
        )
        #'(call/cc (lambda (Break)
            (define (loop alist)
                (when alist
                    (define-values (Var ...) (apply values (map cdr alist)))
                    (call/cc (lambda (Skip)
                        Body ...
                    ))
                    (loop (%more))
                ))
            (loop (%which [Var ...] Goal ...))
        ))
    ]
)

;;  See the limitations of %for-each.

(define-syntax-parser %FOR-EACH
    #:datum-literals (SATISFYING TO BREAK CALL SKIP DO)
    [   (_  (~and-not Var:id SATISFYING) ...
            SATISFYING (~and-not Goal:expr TO) ...
            (~alt
                (~optional (~seq TO BREAK CALL Break:id) #:defaults [(Break #'_break)])
                (~optional (~seq TO SKIP CALL Skip:id) #:defaults [(Skip #'_skip)])
            ) ...
            DO Body ...
        )
        #'(%for-each Var ...
            #:satisfying Goal ...
            #:to-break-call Break
            #:to-skip-call Skip
            #:do Body ...
        )
    ]
)

;;  Like %let, but more verbose and with implicit %and-ing of the body.

(define-syntax-parser %exists
    [(_ Var:id ... #:such-that Goal:expr ...)
        #'(%let (Var ...) (%and Goal ...))
    ])

(define-syntax-parser %EXISTS
    #:datum-literals (SUCH THAT)
    [(_ (~and-not Var:id SUCH) ... SUCH THAT Goal:expr ...)
        #'(%exists Var ... #:such-that Goal ...)
    ])
