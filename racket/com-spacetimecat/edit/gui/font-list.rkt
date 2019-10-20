#lang s-exp "lang.rkt"

(provide
    my-font-list%
)

(define my-font-list% (class font-list% (super-new)
    (define/public (bold-of font)
        (send this find-or-create-font
            (send font get-size)
            (send font get-family)
            (send font get-style)
            'bold
            (send font get-underlined)
            (send font get-smoothing)
            (send font get-size-in-pixels)
            (send font get-hinting)
        ))
))
