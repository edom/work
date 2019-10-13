#lang s-exp "lang.rkt"

(provide
    quick-open-dialog%
)

(define quick-open-dialog% (class dialog%
    (super-new
        [label "Quick Open"]
        [width 640]
        [stretchable-height #t]
    )

    (define dialog this)

    (define text-field (new text-field%
        [label "Pattern"]
        [parent dialog]
        [callback (λ c e ->
            (dynamic-wind
                (λ => send result-panel begin-container-sequence)
                (λ ->
                    (define query (send c get-value))
                    (send result-panel change-children (λ _ -> '()))
                    (send dialog resize 640 (send dialog min-height))
                    (when (non-empty-string? query)
                        (update-result-for query))
                )
                (λ => begin
                    (send result-panel end-container-sequence)
                    (send dialog reflow-container)
                )
            )
        )]
    ))

    (define result-panel (new vertical-panel%
        [parent dialog]
        [alignment '(left top)]
        [stretchable-height #t]
    ))

    (define (update-result-for query)
        (define paths (find-paths-for-quick-open))
        (define pairs (match-paths #:in paths #:according-to query))
        (define results (list-take-at-most 16 #:from pairs))
        (define matches (map cdr results))
        (for-each make-result-item matches)
    )

    (define (make-result-item match)
        (define make-pair cons)
        (define span? pair?)
        (define (make-span begin end) (make-pair begin end))
        (define span-begin car)
        (define span-end cdr)
        (define string (Match-string match))
        (define highlight-indexes (Match-indexes/ascending match))
        (define normal-indexes (for/list [
                (i (in-range (string-length string)))
                #:when (not (member i highlight-indexes))
            ]
            i))
        (define (group-adjacent-indexes indexes)    ;; : [[Index]]
            (define (loop span indexes)             ;; : [Index] -> [Index] -> [Span]
                (if (null? indexes)
                    (list span)
                    (let (
                            [begin (span-begin span)]
                            [end (span-end span)]
                            [ind (car indexes)]
                            [rest (cdr indexes)]
                        )
                        (if (= end ind)
                            (loop (make-span begin (+ end 1)) rest)
                            (cons span (loop (make-span ind (+ ind 1)) rest))
                        )
                    )
                )
            )
            (if (null? indexes)
                (list (make-span 0 0))
                (let (  [ind (car indexes)]
                        [rest (cdr indexes)]
                    )
                    (loop (make-span ind (+ ind 1)) rest))))
        (define highlight-spans (group-adjacent-indexes highlight-indexes))
        (define normal-spans (group-adjacent-indexes normal-indexes))
        (define type-span-list
            (list-sort
                (list-append
                    (list-map (λ s => cons 'normal s) normal-spans)
                    (list-map (λ s => cons 'highlight s) highlight-spans)
                )
                <
                #:key (λ a => span-begin (cdr a))
                #:cache-keys? #f
            ))
        (define line (new horizontal-panel%
            [parent result-panel]
            [alignment '(left center)]
            [stretchable-height #f]
        ))
        (define highlight-font
            (send the-font-list find-or-create-font
                (send normal-control-font get-size)
                (send normal-control-font get-family)
                (send normal-control-font get-style)
                'bold
                (send normal-control-font get-underlined)
                (send normal-control-font get-smoothing)
                (send normal-control-font get-size-in-pixels)
                (send normal-control-font get-hinting)
            )
        )
        (send line begin-container-sequence)
        (for-each
            (λ type-span ->
                (let* [
                        (type (car type-span))
                        (span (cdr type-span))
                        (font (case type
                            [(normal) normal-control-font]
                            [(highlight) highlight-font]
                        ))
                    ]
                    (new message%
                        [label (substring string (span-begin span) (span-end span))]
                        [font font]
                        [parent line]
                        [horiz-margin 0]
                    )))
            type-span-list
        )
        (send line end-container-sequence)
    )

    (define/override (on-activate active?)
        (when active? (send text-field focus)))
))
