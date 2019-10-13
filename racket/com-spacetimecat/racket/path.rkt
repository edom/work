#lang s-exp "lang.rkt"

(require+provide
    racket/path
)

(provide
    pathy->string
    split-path/string
    path-get-extension/utf-8
    path-remove-trailing-slash
)

(define (pathy->string x)
    (if (path? x)
        (path->string x)
        ""
    ))

(define/contract (split-path/string path)
    (-> path? (values string? string?))
    (define-values (parent name _dirlike?) (split-path path))
    (values
        (pathy->string parent)
        (pathy->string name)
    ))

(define (path-get-extension/utf-8 path #:or (fail #f))
    (define ext (path-get-extension path))
    (if ext
        (bytes->string/utf-8 ext)
        fail
    ))

(define (path-remove-trailing-slash path)
    (define-values (parent name _dir?) (split-path path))
    (if (equal? parent 'relative)
        (build-path name)
        (build-path parent name)
    )
)
