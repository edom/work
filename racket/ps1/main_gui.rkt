#lang racket

(provide (all-defined-out))

(require "main.rkt")

(require stc-racket/racket-extra)
(require racket/gui/base)

(define frame (new frame% [label "PS1"]))
(new message% [parent frame] [label "Foobarbaz"])
(new button% [parent frame] [label "Pause"] [callback (λ button event => pause)])
(new button% [parent frame] [label "Resume"] [callback (λ button event => resume)])
(send frame show #t)
