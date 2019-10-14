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
    (init-field [selected #f])
    (init-field [user-data #f])

    (define/public (get-user-data) user-data)
    (define/public (set-user-data x) (set! user-data x))

    (define label-markup markup)

    (super-new
        [alignment '(left center)]
    )

    ;;  ■ U+25A0 black square
    (define selection-indicator-string "■")

    (define (compute-markup)
        (if selected
            (m:hflow (list (m:string selection-indicator-string) label-markup))
            label-markup
        )
    )

    (define canvas (new markup-canvas%
        [parent this]
        [markup (compute-markup)]
    ))

    (forward-method canvas (suspend-flush))
    (forward-method canvas (resume-flush))

    (define/public (get-selected) selected)

    (define/public (set-selected s)
        (unless (equal? selected s)
            (set! selected s)
            (refresh)
        )
    )

    (define (toggle-selected) (set-selected (not (get-selected))))

    (define/public (set-markup m)
        (set! label-markup m)
        (refresh)
    )

    (define (refresh)
        (send canvas set-markup (compute-markup))
    )

    (define/override (on-subwindow-event r e)
        (when (send e button-down? 'left) (toggle-selected))
        (super on-subwindow-event r e)
    )
))
