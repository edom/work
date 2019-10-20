#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
    "option-data.rkt"
    "markup-view.rkt"
)

(provide
    option-item<%>
    option-item%
    make-option-item
)

(define index? exact-nonnegative-integer?)

;;  This is about the appearance.
;;  Key is for search.
;;  Label is for display.
(define option-item<%> (interface ()
    (get-key                    (->m string?))
    (get-value                  (->m any/c))
    (get-selected               (->m boolean?))
    (set-selected               (->m boolean? any/c))
    (toggle-selected            (->m any/c))
    (get-highlight-indexes      (->m (listof index?)))
    (set-highlight-indexes      (->m (listof index?) any/c))
))

(define option-item% (class* horizontal-panel% (option-item<%>)
    (init-field [data (make-option "" #f)])
    (init       [parent dummy-parent-frame])
    (super-new  [parent parent]
                [alignment '(left center)]
                [stretchable-height #f])

    (define     highlight-indexes       '())

    ;;  Static Aspects

    (define     check-box   (new check-box% [parent this] [label ""] [vert-margin 0] [horiz-margin 0]))
    (define     canvas      (new markup-canvas% [parent this] [markup (recompute-markup)]))

    (define/public      (get-key)                   (Option-key data))
    (define/public      (get-value)                 (Option-value data))
    (define/private     (get-label)                 (send canvas get-markup))
    (define/private     (set-label x)               (send canvas set-markup x))
    (define/private     (recompute-markup)          (compute-markup (get-key) highlight-indexes))

    ;;  Dynamic Aspects

    (define/public      (get-selected)              (send check-box get-value))
    (define/public      (set-selected s)            (send check-box set-value s))
    (define/public      (toggle-selected)           (set-selected (not (get-selected))))
    (define/public      (get-highlight-indexes)     highlight-indexes)

    (define/public (set-highlight-indexes x)
        (set! highlight-indexes x)
        (set-label (recompute-markup )))

    (define/forward     (suspend-flush)     canvas)
    (define/forward     (resume-flush)      canvas)

    (define/override (on-subwindow-event r e)
        (and (not (super on-subwindow-event r e))
             (send e button-down? 'left)
             (begin (toggle-selected) #t)))
))

(struct Span (begin end))
(define empty-Span (Span 0 0))

(define (compute-markup string highlight-indexes)
    ;;  This is easier to write but less efficient.
    ;;  However, it would be harder to write but more efficient to use span-subtract,
    ;;  which could be defined in terms of span-invert and span-intersect.
    (define normal-indexes (for/list [
            (i (in-range (string-length string)))
            #:when (not (member i highlight-indexes))
        ]
        i))
    (define highlight-spans (group-adjacent-indexes highlight-indexes))
    (define normal-spans (group-adjacent-indexes normal-indexes))
    (define (substr span) (substring string (Span-begin span) (Span-end span)))
    (define span-markup-pair
        (list-sort
            (list-append
                (list-map (λ s -> (cons s (m:string (substr s)))) normal-spans)
                (list-map (λ s -> (cons s (m:bold (m:string (substr s))))) highlight-spans)
            )
            <
            #:key (λ a -> (Span-begin (car a)))
            #:cache-keys? #f
        ))
    (m:hflow (list-map cdr span-markup-pair)))

;;  The input indexes must be ordered ascending.
(define (group-adjacent-indexes indexes)    ;; : [[Index]]
    (define (loop span indexes)             ;; : [Index] -> [Index] -> [Span]
        (if (null? indexes)
            (list span)
            (let  ( [begin (Span-begin span)]
                    [end (Span-end span)]
                    [ind (car indexes)]
                    [rest (cdr indexes)] )
                (if (= end ind)
                    (loop (Span begin (+ end 1)) rest)
                    (cons span (loop (Span ind (+ ind 1)) rest))))))
    (if (null? indexes)
        (list empty-Span)
        (let  ( [ind (car indexes)]
                [rest (cdr indexes)] )
            (loop (Span ind (+ ind 1)) rest))))

(define/contract (make-option-item parent key meaning)
    (-> (is-a?/c area-container<%>) string? any/c (is-a?/c option-item<%>))
    (new option-item% [parent parent] [data (make-option key meaning)]))
