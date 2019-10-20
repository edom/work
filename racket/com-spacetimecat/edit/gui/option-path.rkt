#lang s-exp "lang.rkt"

(require
    "../quick-open.rkt"
    "option-item.rkt"
    "option-list.rkt"
    "worker.rkt"
)

(provide
    path-option-list%
    path-option-dialog%
)

;;  Query the file system.
(define path-option-list% (class combo-option-list%
    (init-field [before-options-change (λ -> (void))]
                [after-options-change (λ -> (void))]
                [after-submit (λ answers -> (void))])
    (super-new  [query-label "Pattern"]
                [choose-label "Open"]
                [after-query-change (λ old query -> (i-after-query-change old query))]
                [after-submit (λ -> (internal-on-commit))])

    (define (i-after-query-change old query)
        (send this remove-all-options)
        (before-options-change)
        (when (non-empty-string? query) (send worker set-input query)))

    (define (compute input)
        (find-quick-open-paths-for input))

    (define (after-compute input output)
        (for ([elem (in-list output)])
            (define path (car elem))
            (define the-match (cdr elem))
            (define option (send this add-option (path->string path) path))
            (send option set-highlight-indexes (Match-indexes/ascending the-match)))
        (after-options-change))

    (define worker (make-worker #:compute compute #:after-compute after-compute))

    (define (get-children/try-more)
        (define children (send this get-choices))
        (if (null? children)
            (begin (send this try-set-chosen-indexes '(0))
                   (send this get-choices))
            children))

    (define (internal-on-commit)
        (define children (get-children/try-more))
        (define paths (list-map (λ c -> (send c get-value)) children))
        (after-submit paths))
))

(define (find-quick-open-paths-for query)
    (define all-paths (find-paths-for-quick-open))
    (define pairs (match-paths #:in all-paths #:according-to query))
    (define limit 16)
    (list-take-at-most limit #:from pairs))

(define default-dialog-width 640)

;;  2019-10-20: Design Question: Should this be named choose-path-dialog% instead?
;;  2019-10-20: Design Thought: There should be just one combo-option-dialog% with customizable matching and option items.
(define path-option-dialog% (class dialog%
    (init-field [(i-after-submit after-submit) (λ paths -> #t)])
    (init       [label "Choose Path"])
    (super-new  [label label]
                [width default-dialog-width]
                [stretchable-height #t])

    (define option-list (new path-option-list% [parent this]
        [before-options-change (λ -> (send this resize default-dialog-width (send this min-height)))]
        [after-options-change (λ -> (send this reflow-container))]
        [after-submit (λ paths ->
            (when (and (not (list-empty? paths)) (i-after-submit paths))
                (send this show #f)
            ))]))

    (define/override (on-activate active?) (when active? (send option-list focus-on-query-field)))

    (define/public (clear-query) (send option-list set-query ""))
))

;;  Test.
#;(begin
    (provide main)
    (define (main)
        (define frame (new test-frame%))
        (define dialog (new path-option-dialog% [parent frame]))
        (send dialog show #t)
        (void)))
