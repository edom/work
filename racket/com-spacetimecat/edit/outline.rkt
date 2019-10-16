#lang s-exp "lang.rkt"

(require
    "../racket/list.rkt"
    racket/match
    "outline-syntax.rkt"
    (for-syntax "outline-syntax.rkt")
)

(provide

    outline-node/c
    outline-node-list/c

    compute-file-outline

    outline-node-type
    outline-node-target
    outline-node-children

)

;;  The shape of the return value is Parts:
;;
;;      Parts       ::= (Part ...)
;;      Part        ::= Type Children
;;      Type        ::= (module Id Init)
;;                  |   (require Spec)
;;                  |   (provide Spec)
;;                  |   (variable Id)
;;                  |   (procedure Id)
;;                  |   (parameter Id)
;;                  |   (transformer Id)
;;                  |   (class Super)
;;                  |   (define-class Id Super)
;;      Children    ::= Parts
;;
;;  This is only an approximation.
;;  This is the 20% effort that works for 80% Racket programs.
;;
;;  We don't use #:literals because it requires computing the bindings
;;  which requires fully-expanding the module.

(define outline-type/c any/c)
(define outline-target/c (or/c syntax? #f))
(define outline-node/c (list/c
    outline-type/c
    outline-target/c
    [recursive-contract outline-node-list/c]
))
(define outline-node-list/c (listof [recursive-contract outline-node/c]))

(define (outline-node-type x) (list-ref x 0))
(define (outline-node-target x) (list-ref x 1))
(define (outline-node-children x) (list-ref x 2))

;;  Design Notes: 2019-10-16:
;;
;;  I think outlining should be done after the bindings are computed (after the requires are resolved)
;;  but before the defines are expanded.
;;  This is because, for example, the exact expansion of racket/class's class form
;;  is an internal detail that we should avoid depending on.
;;
;;  Modules with side effects greatly complicate reasoning.
;;  But the majority of Racket modules do not have side effects when required.
;;  Thus we assume that modules do not have side effects when they are required.
;;
;;  Modules should not have side effects when they required.
;;  Let application writers initialize the global variables of C libraries explicitly in their main methods,
;;  not in module require/visit/load time.

;;  ($outline Type Id Unexpanded-Children)
;;  Only for template.
;;  The syntax-source/position of Id is the definition site.
(define $outline #f)
(define $literal #f)

(define _interpret #f)
(define _expand #f)
(define _nothing #f)

(struct Node (phase site type children) #:prefab)

(define/contract (expand-top stx)
    (-> syntax? Node?)
    (define p0 0)
    (syntax-parse stx
        #:datum-literals (module #%module-begin)
        [(module Id:id Init (#%module-begin . Body))
        (Node p0 #'Id (list 'module #'Id #'Init) (expand-body p0 #'Body))]))

(define/contract (expand-body phase stx)
    (-> integer? syntax? (listof Node?))
        (list-append-map (λ s => expand-form phase s) (syntax->list stx)))

(define/contract (expand-form p0 stx)
    (-> integer? syntax? (listof Node?))
    (define p1 (+ p0 1))
    (define/contract (proc node)
        (-> (or/c Node? #f) (listof Node?))
            (match node
                [(Node phase site type children)
                    (list (Node
                        phase
                        site
                        (syntax->datum type)
                        children
                    ))]
                [#f '()]))
    (syntax-parse stx
        #:datum-literals (
            require provide
            begin begin-for-syntax
            define-syntax define-for-syntax
            class class*
        )
        [(begin . Body)                                 (expand-body p0 #'Body)]
        [(begin-for-syntax . Body)                      (expand-body p1 #'Body)]
        [(require Spec ...)                             (list-append-map (λ f => proc (expand-form-1 p0 f)) (syntax->list #'((require Spec) ...)))]
        [(provide Spec ...)                             (list-append-map (λ f => proc (expand-form-1 p0 f)) (syntax->list #'((provide Spec) ...)))]
        [thing                                          (proc (expand-form-1 p0 #'thing))]))

(define (expand-form-1 p0 stx)
    (define p1 (+ p0 1))
    (syntax-parse stx
        #:datum-literals (
            require provide
            begin begin-for-syntax
            define-syntax define-for-syntax
            class class*
        )
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    (bind-phase defn-site node-type children)
        [(require Spec)                                 (Node p0 #'Spec #'(require Spec) '())]
        [(provide Spec)                                 (Node p0 #'Spec #'(provide Spec) '())]
        [form:define-class-form$                        (Node p0 #'form.id #'(define form.id (class form.super)) (expand-body p0 #'(form.body ...)))]
        [(define-for-syntax (Id:id . Params) . Body)    (Node p1 #'Id #'(define Id (procedure Params)) (expand-body p1 #'Body))]
        [(_:define$ Id:id Init)                         (Node p1 #'Id #'(define Id variable) (expand-body p1 #'(Init)))]
        [(_:define$ (Id:id . Params) . Body)            (Node p0 #'Id #'(define Id (procedure Params)) (expand-body p0 #'Body))]
        [(_:define$ Id:id Init)                         (Node p0 #'Id #'(define Id variable) (expand-body p0 #'(Init)))]
        [(define-syntax (Id:id . Params) . Body)        (Node p0 #'Id #'(define Id (transformer-procedure Params)) (expand-body p1 #'Body))]
        [(define-syntax Id:id . Body)                   (Node p0 #'Id #'(define Id transformer-variable) (expand-body p1 #'Body))]
        [(class Super:expr . Body)                      (Node p0 #'Super #'(class Super) (expand-body p0 #'Body))]
        [(class* Super:expr _Ifaces:expr . Body)        (Node p0 #'Super #'(class Super) (expand-body p0 #'Body))]
        [_                                              #f]))

(define (outline-syntax->datum lst)
    (define (f1 stx)
        (match (syntax->list stx)
            [(list phase site type children)
                (list   phase
                        (syntax-source site)
                        (syntax-position site)
                        (syntax->datum type)
                        (outline-syntax->datum children))
            ]))
    #f)

;;  See compute-module-outline for the return value.

(define (compute-file-outline path)
    (define mod
        (parameterize (
                (read-accept-lang #t)
                (read-accept-reader #t)
            )
            (read-syntax-from-file path)
        ))
    (compute-module-outline mod)
    ;; TODO: Replace with (expand-top mod)
)

(define/contract (compute-module-outline stx)
    (-> syntax? outline-node-list/c)
    (define (d stx) (syntax->datum stx))
    (define (loop stx)
        ;;  Each case produces a list of parts.
        (syntax-parse stx
            #:datum-literals (
                module #%module-begin require provide
                begin begin-for-syntax
                define-syntax
                class class*
            )
            [   (module Id:id Init (#%module-begin Body ...))
                `([ ,(d #'(module Id Init))
                    ,#'Id
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (begin)
                '()
            ]
            [   (begin Body1 Body2 ...)
                `(
                    ,@(loop #'Body1)
                    ,@(loop #'(begin Body2 ...))
                )
            ]
            [   (begin-for-syntax Body ...)
                (loop #'(begin Body ...))
            ]
            [   (require . Specs) (map (λ s -> `([require ,(d s)] ,s [])) (syntax->list #'Specs))]
            [   (provide . Specs) (map (λ s -> `([provide ,(d s)] ,s [])) (syntax->list #'Specs))]
            ;;  TODO: lambda
            [   (_:define$ (Id:id . Params) Body ...)
                `([ ,(d #'(procedure Id))
                    ,#'Id
                    (   ,@(map (λ p -> `([parameter ,(d p)] ,p [])) (syntax->list #'Params))
                        ,@(loop #'(begin Body ...))
                    )
                ])
            ]
            [   form:define-class-form$
                `([ ,(d #'(define-class form.id form.super))
                    ,#'form.id
                    ,(loop #'(form.body ...))
                ])
            ]
            [   (_:define$ Id:id Init)
                `([ ,(d #'(variable Id))
                    ,#'Id
                    ,(loop #'Init)
                ])
            ]
            [   (_:define$ Id:id Init)
                `([ (variable ,(d #'Id))
                    ,#'Id
                    ,(loop #'Init)
                ])
            ]
            [   (class Super:expr Body ...)
                `([ (class ,(d #'Super))
                    ,#'Super
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (class* Super:expr _Ifaces:expr Body ...)
                `([ (class ,(d #'Super))
                    ,#'Super
                    ,(loop #'(begin Body ...))
                ])
            ]
            [   (Expr ...) (loop #'(begin Expr ...))]
            [   _ '()]
        )
    )
    (loop stx)
)
