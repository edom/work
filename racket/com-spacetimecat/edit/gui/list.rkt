#lang s-exp "lang.rkt"

(require
    (prefix-in m: "markup.rkt")
    "markup-view.rkt"
)

(provide
    quick-open-choice-list%
    quick-open-choice%
)

(define quick-open-choice-list% (class vertical-panel%
    (super-new [alignment '(left top)])
    (inherit get-children)
    (define/public (clear) (clear-container this))
    (define/public (count-children) (list-length (get-children)))
    (define/public (get-selected-children)
        (list-filter
            (λ c => send c get-selected)
            (get-children)
        )
    )
    (define/public (try-set-selected-indexes list)
        (define children (list->vector (get-children)))
        (for ([child (in-vector children)])
            (send child set-selected #f)
        )
        (define n (vector-length children))
        (for ([i (in-list list)])
            (when (and (<= 0 i) (< i n))
                (define child (vector-ref children i))
                (send child set-selected #t)
            )
        )
    )
))

(define quick-open-choice% (class horizontal-panel%
    (init [markup (m:empty)])
    (init-field [user-data #f])
    (super-new [alignment '(left center)])

    ;;  Static Aspects

    (define check-box (new check-box% [parent this] [label ""] [vert-margin 0] [horiz-margin 0]))
    (define canvas (new markup-canvas% [parent this] [markup markup]))

    (define/public (get-user-data) user-data)
    (define/public (set-user-data x) (set! user-data x))

    (forward-method canvas (suspend-flush))
    (forward-method canvas (resume-flush))

    ;;  Dynamic Aspects

    (forward-method canvas (set-markup))

    (define/public (get-selected) (send check-box get-value))
    (define/public (set-selected s) (send check-box set-value s))
    (define (toggle-selected) (set-selected (not (get-selected))))

    (define/override (on-subwindow-event r e)
        (when (and  (not (super on-subwindow-event r e))
                    (send e button-down? 'left))
            (toggle-selected))
        #t
    )
))
