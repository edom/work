#lang s-exp "../racket/lang.rkt"

(provide
    (all-from-out "../racket/lang.rkt")
)

(require+provide
    racket/contract/base
    racket/contract/region
    "../racket/fewer-parens.rkt"
    "../racket/string.rkt"
    "../racket/class.rkt"
)
