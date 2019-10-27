#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
    "markup-view.rkt"
    "option-data.rkt"
    "option-item.rkt"
    "worker.rkt"
)

(provide
    combo-option-list<%>
    combo-option-list%
    combo-option-dialog%
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

    (define/public (get-choices/try-harder)
        (define children (send this get-choices))
        (if (null? children)
            (begin (send this try-set-chosen-indexes '(0))
                   (send this get-choices))
            children))
))

;;  2019-10-22: Design Decision: combo-option-list% does not subclass option-list%.
;;
;;  In reality, a combo-option-list% is a vertical-panel%, not an option-list%.
;;
;;  However, practically, users expect combo-option-list% to understand a superset of
;;  the messages understood by option-list%.
;;
;;  Thus, to make both worlds happy, a combo-option-list% is an option-list<%>
;;  but not at option-list%.
;;
;;  Even with multiple inheritance, it does not make sense for combo-option-list%
;;  to inherit from both vertical-panel% and option-list%.
;;
;;  Events:
;;
;;      (after-query-change Old New)
;;      (before-options-change)
;;      (after-options-change)
;;      (after-submit Values)           ;; the value, not the option-item<%>
;;
;;  Before that, each event handler was a field that contains a lambda,
;;  but that hampers subclasses from intercepting/augmenting event handling.
(define combo-option-list% (class* vertical-panel% (combo-option-list<%>)
    (init       [query-label "Pattern"]
                [choose-label "Choose"])
    (super-new  [alignment '(left top)]
                [stretchable-height #f])

    ;;  Event.
    (define/public (handle-event e)
        (match e
            [`(after-query-change ,old ,query)
                (send this remove-all-options)
                (send this handle-event `(before-options-change))
                (when (non-empty-string? query)
                    (send this begin-compute query))]
            [_ (void)]))

    (define/public (begin-compute input) (send worker set-input input))
    (define/public (compute-options-for query) (list query))
    (define/public (after-compute input output)
        (list-for-each (λ string -> (send this add-option string string)) output)
        (send this handle-event `(after-options-change)))

    (define worker (make-worker
        #:compute (λ q -> (send this compute-options-for q))
        #:after-compute (λ q r -> (send this after-compute q r))))

    ;;  Appearance.
    (define top-bar (new horizontal-panel% [parent this]))
        (define query-field (new text-field% [label query-label] [parent top-bar]
            [callback (λ c e -> (case (send e get-event-type)
                    [(text-field) (internal-on-change)]
                    [(text-field-enter) (after-submit)]
                ))]))
        (define choose-button (new button% [label choose-label] [parent top-bar]
            [callback (λ b e -> (after-submit))]))
    (define option-list (new option-list% [parent this]))

    (define/private (after-submit)
        (define choices (send option-list get-choices/try-harder))
        (define values (list-map (λ c -> (send c get-value)) choices))
        (when (not (null? values))
            (handle-event `(after-submit ,values))))

    (define/public (focus-on-query-field) (send query-field focus))
    (define/public (get-query) (send query-field get-value))
    (define/public (set-query q)
        (send query-field set-value q)
        (internal-on-change)) ;; set-value does not call callback.

    (define previous-query "")
    (define (internal-on-change)
        (with-container-sequence option-list
            (define query (get-query))
            (handle-event `(after-query-change ,previous-query ,query))
            (set! previous-query query)))

    ;;  Forward option-list<%> methods to option-list.
    (define/forward (add-option key value) option-list)
    (define/forward (get-options) option-list)
    (define/forward (get-choices) option-list)
    (define/forward (remove-all-options) option-list)
    (define/forward (try-set-chosen-indexes indexes) option-list)
))

(define default-dialog-width 640)

(define combo-option-dialog% (class* dialog% (combo-option-list<%>)
    (init       [label "Choose Path"]
                [width default-dialog-width])
    (super-new  [label label]
                [width width]
                [stretchable-height #t])
    (init-field [option-list (make-option-list)])

    (define initial-width width)

    (define/public (after-submit answers) #t)

    ;;  Subclass may override this method.
    (define/public (get-option-list-class) combo-option-list%)

    ;;  Subclass may override this method.
    ;;  This should return an option-list<%>.
    (define/public (make-option-list)
        (define dialog this)
        (new (class (get-option-list-class)
            (super-new)
            (define/override (handle-event e)
                (super handle-event e)
                (match e
                    [`(before-options-change)
                        (send dialog resize initial-width (send dialog min-height))]
                    [`(after-options-change)
                        (send dialog reflow-container)]
                    [`(after-submit ,paths)
                        (when (and (not (list-empty? paths)) (after-submit paths))
                            (send dialog show #f))]
                    [_ (void)])))
            [parent dialog]))

    (define/override (on-activate active?)
        (when active? (send option-list focus-on-query-field)))

    ;;  Forward option-list<%> methods to option-list.
    (define/forward (add-option key value) option-list)
    (define/forward (get-options) option-list)
    (define/forward (get-choices) option-list)
    (define/forward (remove-all-options) option-list)
    (define/forward (try-set-chosen-indexes indexes) option-list)
    ;;  Forward combo-option-list<%> methods to option-list.
    (define/forward (get-query) option-list)
    (define/forward (set-query str) option-list)
    (define/forward (focus-on-query-field) option-list)
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
