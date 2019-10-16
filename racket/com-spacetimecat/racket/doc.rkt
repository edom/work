#lang racket/base

;;  In-source structured documentation system.
;;
;;  The key idea:
;;  In normal execution, $doc forms expand into (begin), that is, do-nothing.
;;  In documentation generation, we collect the $doc forms.
;;
;;  Adding $doc forms should have negligible expansion-time cost
;;  and zero run-time cost.

(require
    (for-syntax
        racket/base
    )
    racket/format
    racket/list
    syntax/strip-context
    "syntax.rkt"
)

(provide
    TODO
    $doc
    $region
    read-syntax-from-file
    read-docs-from-file
)

(define-syntax-rule (TODO Arg ...) (error 'TODO Arg ...))

(define-syntax-rule ($doc Arg ...) (begin))

(define-syntax-rule ($region Label Arg ...) (begin Arg ...))

;;  See also: read-accept-lang and read-accept-reader.

(define (read-syntax-from-file path)
    (parameterize [(port-count-lines-enabled #t)]
        (call-with-input-file path (lambda (port)
            (read-syntax path port)
        ))))

;;  This is insecure; a #lang or #reader may do anything.
;;  Do not run this on source files you do not trust.
;;  Perhaps we should sandbox the reading?

(define (read-docs-from-file path)
    (define mod/read (parameterize [
            (read-accept-lang #t)
            (read-accept-reader #t)
        ]
        (read-syntax-from-file path)
    ))
    (define mod/expanded
        ;;  XXX: How do we expand the module in the same way Racket expands it
        ;;  to produce binding information?
        (replace-context #'read-docs-from-file mod/read)
    )
    (extract-docs-from-module path mod/expanded)
)

(define (extract-docs-from-module path stx)
    (syntax-parse stx
        #:literals (module #%module-begin)
        [   (module Mod-Id Lang
                (#%module-begin
                    Stmt ...
                )
            )
            (append-map extract-docs-from-statement (syntax->list #'(Stmt ...)))
        ]
        [   _
            (error 'extract-docs-from-module "input file is not a Racket module: ~a" path)
        ]
    )
)

(define current-procedure-id (make-parameter #f))

(struct Doc (target content) #:prefab)

(define (extract-docs-from-statement stx)
    (syntax-parse stx
        #:literals ($doc define)
        [   ($doc #:for Id Arg ...)
            (list (Doc #'Id #'(Arg ...)))
        ]
        [   ($doc Arg ...)
            (define proc (current-procedure-id))
            (if proc
                (list (Doc (current-procedure-id) #'(Arg ...)))
                (error "$doc without #:for cannot be used outside named procedure body")
            )
        ]
        [   (define (Id Arg ...) Body ...)
            (parameterize [(current-procedure-id #'Id)]
                (append-map extract-docs-from-statement (syntax->list #'(Body ...)))
            )
        ]
        [   _
            (list)
        ]
    )
)