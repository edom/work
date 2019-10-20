#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
    "markup-view.rkt"
    "option-data.rkt"
    "option-item.rkt"
)

(provide
    combo-option-list<%>
    combo-option-list%
)

(define query? string?)
(define option-item/c (is-a?/c option-item<%>))

(define option-list<%> (interface ()
    (add-option                 (->m string? any/c option-item/c))
    (get-options                (->m (listof option-item/c)))
    (get-choices                (->m (listof option-item/c)))
    (remove-all-options         (->m any/c))
    ;;  Replace, not augment, the set of choices.
    (try-set-chosen-indexes     (->m (listof exact-nonnegative-integer?) any/c))
))

;;  Appearance: A text field and an option list.
;;  Intended Subclass Behavior: Fuzzy-string-match the option keys as you type.
;;  Use case: The user wants to choose one or some of many options
;;  (paths, words, lines, identifiers) with string labels.
(define combo-option-list<%> (interface (option-list<%>)
    (get-query                  (->m query?))
    (set-query                  (->m query? any/c))
    (focus-on-query-field       (->m any/c))
    ;;  This method should be overridden.
    ;;  This runs in a non-GUI thread, so it must not use any GUI methods.
    ;compute-answer-for          ;;  -> query? answer?
    ;;  This is for display.
    ;answer-label                ;;  -> answer? string?
    ;answer-highlighted-indexes  ;;  -> answer? (listof exact-nonnegative-integer?)
    ;;  This can contain any user data.
    ;answer-meaning              ;;  -> answer? any/c
))

;;  Do not manually add children such as by (new something% [parent an-option-list]).
;;  Use add-option.
;;
;;  Notes on terminology:
;;  Options are what the user can choose.
;;  Choices are what the user has chosen.
(define option-list% (class* vertical-panel% (option-list<%>)
    (super-new [alignment '(left top)]
               [stretchable-height #f])

    (define/public (add-option key value) (make-option-item this key value))
    (define/public (remove-all-options) (clear-container this))
    (define/public (get-options) (send this get-children))
    (define/public (get-choices)
        (list-filter
            (λ c -> (send c get-selected))
            (get-options)))
    (define/public (try-set-chosen-indexes list)
        (define children (list->vector (get-options)))
        (for ([child (in-vector children)]) (send child set-selected #f))
        (define n (vector-length children))
        (for ([i (in-list list)])
            (when (and (<= 0 i) (< i n))
                (define child (vector-ref children i))
                (send child set-selected #t))))
))

(define combo-option-list% (class* vertical-panel% (option-list<%>)
    (init      [query-label "Pattern"]
               [choose-label "Choose"]
               [after-query-change (λ old new -> (void))]
               [after-submit void])
    (super-new [alignment '(left top)]
               [stretchable-height #f])

    ;;  Appearance.
    (define top-bar (new horizontal-panel% [parent this]))
        (define query-field (new text-field% [label query-label] [parent top-bar]
            [callback (λ c e -> (case (send e get-event-type)
                    [(text-field) (internal-on-change)]
                    [(text-field-enter) (after-submit)]
                ))]))
        (define choose-button (new button% [label choose-label] [parent top-bar] [callback (λ b e -> (after-submit))]))
    (define option-list (new option-list% [parent this]))

    (define/public (focus-on-query-field) (send query-field focus))
    (define/public (get-query) (send query-field get-value))
    (define/public (set-query q)
        (send query-field set-value q)
        (internal-on-change)) ;; set-value does not call callback.

    (define previous-query "")
    (define (internal-on-change)
        (with-container-sequence option-list
            (define query (get-query))
            (after-query-change previous-query query)
            (set! previous-query query)))

    ;;  Forward option-list<%> methods to option-list.
    (define/forward (add-option key value) option-list)
    (define/forward (get-options) option-list)
    (define/forward (get-choices) option-list)
    (define/forward (remove-all-options) option-list)
    (define/forward (try-set-chosen-indexes indexes) option-list)
))

;;  Test.
#;(begin
    (provide main)
    (define (main)
        (define frame (new test-frame%))
        (define list (new combo-option-list% [parent frame]))
        (send list set-query "ir")
        (send+ list [add-option "First" "First"] [set-highlight-indexes '(1 2)])
        (send list add-option "Second" "Second")
        (send+ list [add-option "Third" "Third"] [set-highlight-indexes '(2 3)])
        (void)))
