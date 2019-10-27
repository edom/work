#lang s-exp "lang.rkt"

(require
    "../quick-open.rkt"
    "option-item.rkt"
    "option-list.rkt"
)

(provide
    path-option-list%
    path-option-dialog%
)

;;  Query the file system.
(define path-option-list% (class combo-option-list%
    (super-new [query-label "Pattern"]
               [choose-label "Open"])

    (define/override (compute-options-for input) (find-quick-open-paths-for input))

    (define/override (after-compute input output)
        (for ([elem (in-list output)])
            (define path (car elem))
            (define the-match (cdr elem))
            (define option (send this add-option (path->string path) path))
            (send option set-highlight-indexes (Match-indexes/ascending the-match)))
        (send this handle-event `(after-options-change)))
))

(define (find-quick-open-paths-for query)
    (define all-paths (find-paths-for-quick-open))
    (define pairs (match-paths #:in all-paths #:according-to query))
    (define limit 16)
    (list-take-at-most limit #:from pairs))

(define default-dialog-width 640)

;;  2019-10-20: Design Question: Should this be named choose-path-dialog% instead?
;;  2019-10-20: Design Thought: There should be just one combo-option-dialog% with customizable matching and option items.
(define path-option-dialog% (class combo-option-dialog%
    (init       [label "Choose Path"]
                [width default-dialog-width])
    (super-new  [label label]
                [width width])

    (define/override (get-option-list-class) path-option-list%)
))

;;  Test.
#;(begin
    (provide main)
    (define (main)
        (define frame (new test-frame%))
        (define dialog (new path-option-dialog% [parent frame]))
        (send dialog show #t)
        (void)))
