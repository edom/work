#lang s-exp "lang.rkt"

;;  The idea is to make an inspectable GUI.
;;  This element is designed to provide a quick answer to the question
;;  "Where do I have to edit the code if I want to change
;;  the appearance or the behavior of this GUI thing?"
;;
;;  The idea is to make a UI spy.
;;
;;  The problems:
;;
;;      -   How do we retrofit this into existing library classes in racket/gui/base?
;;
;;  Problem:
;;  Cannot require for-syntax racket/gui/base because:
;;      cannot instantiate `racket/gui/base' a second time in the same process
;;
;;  Wanted: GUI library without global variables, without singleton initialization.
;;  Something like (new frame% [world world]) or (send world new-frame).

(provide
    element%
)

(define-syntax-rule (define-gui-class id superclass body ...)
    (define id
        (make-inspectable
            (class superclass
                body ...
            )
            #'id
        )))

(define (make-inspectable superclass id-stx)
    (define (inspect)
        (displayln (~a
            "This element is defined"
            'at (syntax-source id-stx)
            'line (syntax-line id-stx)
            'column (syntax-column id-stx)
            (~a "(position " (syntax-position id-stx) ")")
            #:separator " "
        ))
    )
    (cond
        [(implementation? superclass window<%>)
            (class superclass (super-new)
                (define/override (on-event e)
                    (inspect)
                    (super on-event e)
                )
            )
        ]
        [else
            (error 'make-inspectable "unsupported superclass: ~v" superclass)
        ]
    )
)

(define-gui-class element% editor-canvas%
    (super-new [label "Example Element"] [min-width 400] [min-height 400])
)

;;  Test.

(define frame
    (new (class frame% (super-new)
        )
        [label "Element"]
        [width 640]
        [height 480]
    ))
(define element (new element% [parent frame]))
(send frame show #t)
