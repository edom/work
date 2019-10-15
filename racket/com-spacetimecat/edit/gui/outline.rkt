#lang s-exp "lang.rkt"

(require
    racket/format
    racket/match
    mrlib/hierlist
    "../../racket/syntax.rkt"
)

(provide
    outline-view%
)

;;  Problem: 2019-10-12:
;;  hierarchical-list-compound-item<%>:open assumes that its parent is open.
;;  Thus, we open everything first, and then close everything that is not supposed to be open.
;;  But this is extremely slow, even for not-too-large files.
;;
;;  https://github.com/racket/gui/issues/145

(define outline-view% (class vertical-panel%

    (init-field [on-request-open (λ path position => void)])

    (super-new)

    (new message% [label "Outline"] [parent this] [auto-resize #t])

    (define/public (set-outline outline)
        ;;  Problem: 2019-10-13:
        ;;  hierarchical-list% does not provide a method for deleting everything.
        (send this delete-child hlist)
        (set! hlist (new-hlist))
        (populate-from outline)
    )

    (define my-hlist% (class hierarchical-list% (super-new)
        (define/override (on-select item)
            (when item
                (define target (send item user-data))
                (when target
                    (define path (syntax-source target))
                    (define pos0 (syntax-position-0 target))
                    (when (and path pos0)
                        (on-request-open path pos0)
                    )
                )
            )
            (super on-select item)
        )
    ))

    (define (new-hlist) (new my-hlist% [parent this]))
    (define hlist (new-hlist))

    (define (populate-from outline)
        (define (loop1 gui things)
            (for-each (λ thing => loop2 gui thing) things)
        )
        (define/contract (loop2 gui thing)
            (-> any/c outline-node/c any)
            (define (item label target)
                (define item (send gui new-item))
                (define editor (send item get-editor))
                (send item user-data target)
                (send editor insert label)
            )
            (define (section label target cont #:open (want-open #f))
                (define list (send gui new-list))
                (define editor (send list get-editor))
                (send list user-data target)
                (send editor insert label)
                ;;
                ;;  Cannot simply do this:
                ;;
                ;;      (when want-open (send list open))
                ;;
                ;;  See above notes for the problem.
                ;;
                (send list open)
                (cont list)
                (unless want-open (send list close))
            )
            (define type (outline-node-type thing))
            (define target (outline-node-target thing))
            (define children (outline-node-children thing))
            (define label (match type
                [`(module ,Id ,Init) (~s 'module Id Init #:separator " ")]
                [`(procedure ,Id) (~s 'procedure Id #:separator " ")]
                [`(variable ,Id) (~s 'variable Id #:separator " ")]
                [`(class ,Super) (~s 'class Super #:separator " ")]
                [`(require ,Spec) (~s 'require Spec #:separator " ")]
                [`(provide ,Spec) (~s 'provide Spec #:separator " ")]
                [`(parameter ,Id) (~s 'parameter Id #:separator " ")]
                [_ (~s 'unknown type #:separator " ")]
            ))
            (define kind (car type))
            (define want-open (member kind '(module class)))
            (if (null? children)
                (item label target)
                (section label target (λ gui => loop1 gui children) #:open want-open)
            )
        )
        (loop1 hlist outline)
        hlist
    )
))
