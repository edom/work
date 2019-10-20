#lang s-exp "lang.rkt"

(require
    (only-in "markup.rkt" markup?)
    (prefix-in m: "markup.rkt")
    (prefix-in mi: "markup-interpret.rkt")
    "box.rkt"
)

(provide
    markup-canvas%
)

(define markup-canvas% (class canvas%

    ;;  Do not use 'transparent.
    ;;  The text somehow gets drawn twice and overlaps.
    (super-new  [style '(no-focus)])

    (init-field [markup (m:empty)])

    (inherit    get-dc
                min-width
                min-height)

    (define (recompute-min-size)
        (define box (mi:interpret/measure (get-dc) markup))
        (define (ceil x) (inexact->exact (ceiling x)))
        (min-width (ceil (Box-w box)))
        (min-height (ceil (Box-h box))))

    (define/public (get-markup) markup)

    (define/public (set-markup m)
        (unless (m:markup? m) (raise-argument-error 'set-markup "markup?" m))
        (set! markup m)
        (recompute-min-size)
        (send this refresh))

    (recompute-min-size)

    (define/override (on-paint)
        (super on-paint)
        (mi:interpret/render (get-dc) markup))
))

;;  Test.
#;(begin
    (provide main)
    (define (main)
        (define frame (new test-frame%))
        (define markup
            (m:vflow (list
                (m:hflow (list
                    (m:string "This is ")
                    (m:bold (m:string "bold"))
                    (m:string ".")
                ))
                (m:hflow (list
                    (m:string "This is also ")
                    (m:bold (m:string "bold"))
                    (m:string ".")
                ))
                (m:hflow (list
                    (m:string "This is ")
                    (m:bold (m:string "bold"))
                    (m:string " too.")
                ))
            )))
        (define view (new markup-canvas% [parent frame] [markup markup]))
        (void)))
