#lang racket/base

;;  2019-10-12
;;  https://github.com/racket/gui/issues/145

(require
    racket/class
    racket/gui/base
    mrlib/hierlist
)

(provide
    main
)

(define (main)
    (define frame (new frame% [label "hierlist-error.rkt"] [width 800] [height 600]))
    (define hlist (new hierarchical-list% [parent frame]))
    (define hcompound-1 (send hlist new-list))
    (define hcompound-1-1 (send hcompound-1 new-list))
    ;;  This throws an exception.
    (send hcompound-1-1 open)
    (send frame show #t)
)
