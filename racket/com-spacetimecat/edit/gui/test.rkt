#lang s-exp "lang.rkt"

(require
    racket/path
    racket/pretty
    "outline.rkt"
)

(define-syntax ($__FILE__/path stx)
    (datum->syntax stx (syntax-source stx))
)

(define-syntax ($__FILE__/string stx)
    (datum->syntax stx (path->string (syntax-source stx)))
)

(define f 1)
(define (g x) 2)
(define h (λ x -> x))

(define (main)
    (println (parse-shortcut 'C-M-q))
    (println (parse-shortcut 'C-M-Q))
    (define outline (compute-file-outline
        (simplify-path (build-path $__FILE__/path ".." "gui/gui.rkt") #f)
    ))
    (pretty-print outline)
    (define frame (new frame% [label "Test"] [width 800] [height 600]))
    (new-outline-gui #:for outline #:parent frame)
    (send frame show #t)
)

(provide main)
