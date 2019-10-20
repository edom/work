#lang s-exp "../lang.rkt"

(provide (all-from-out "../lang.rkt"))

;;  Problem: 2019-10-13:
;;  I tried "framework", but it slows down startup.

(require+provide
    racket/gui/base
    "../core.rkt"
)

(provide
    test-frame%
    dummy-parent-frame
    call-with-container-sequence
    with-container-sequence
    clear-container
)

(define test-frame% (class frame%
    (super-new [label "Test Frame (Ctrl+Q to quit)"] [width 640] [height 480])
    (send this show #t)
    (define/override (on-subwindow-char w e)
        (define handled? (super on-subwindow-char w e))
        (define ctrl-q? (and (send e get-control-down)
                             (member (send e get-key-code) '(#\Q #\q))))
        (if handled?
            #t
            (if ctrl-q? (send this on-exit) #f)))))

;;  This is a workaround.
;;  The ideal-but-impractical solution is to fix the lower layers to not require a parent init attribute.
;;  Potential Problem: Can this prevent the application from exiting?
(define dummy-parent-frame (new frame% [label "Dummy frame for parent init attribute"]))

(define/contract (call-with-container-sequence container proc)
    (-> (is-a?/c area-container<%>) (-> any/c) any/c)
    (dynamic-wind
        (λ -> (send container begin-container-sequence))
        proc
        (λ -> (send container end-container-sequence))))

(define-syntax-rule (with-container-sequence container body ...)
    (call-with-container-sequence container (lambda () body ...)))

(define (clear-container container)
    (with-container-sequence container
        (send container change-children (λ _ -> '()))))
