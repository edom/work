#lang s-exp "lang.rkt"

(require+provide
    racket/path
)

(provide
    pathy->string
    split-path/string
    path-get-extension/utf-8
    path-remove-trailing-slash
    get-path-element-separator-char
    get-path-element-separator-string
)

(define (pathy->string x)
    (cond
        [(path? x) (path->string x)]
        [(string? x) x]
        [else ""]
    ))

;; FIXME
(define (get-path-element-separator-char)
    #\/
)

(define (get-path-element-separator-string)
    (string (get-path-element-separator-char))
)

(define/contract (split-path/string path)
    (-> path? (values string? string?))
    (define-values (parent name dirlike?) (split-path path))
    (values
        (pathy->string parent)
        (string-append
            (case name
                [(up) ".."]
                [(same) "."]
                [else (if (path? name)
                    (path->string name)
                    (error 'split-path/string "unexpected name: ~s" name))]
            )
            (if dirlike?
                (get-path-element-separator-string)
                ""
            )
        )
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
