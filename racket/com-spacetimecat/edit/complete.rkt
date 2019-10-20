#lang s-exp "lang.rkt"

(require
    racket/file
    "match.rkt"
)

(provide
    (all-from-out "match.rkt")
    match-paths
    order-strings
    find-completions
)

;;  The returned list must not contain a Match that fails
;;  (whose "indexes/ascending" field contains an empty list).

(define/contract
    (match-paths
        #:in candidates
        #:according-to user-input
    )
    (-> #:in (listof path?)
        #:according-to string?
        (listof (cons/c path? Match?))
    )
    (define pairs (list-filter-map
        (λ path ->
            (define path-str (path->string path))
            (define path-elem-sep (get-path-element-separator-char))
            (define i-last-sep (string-last-index-of path-str path-elem-sep))
            (define m-name (if i-last-sep
                (match-string user-input path-str (+ i-last-sep 1))
                (make-empty-Match)))
            (define m-path (match-string user-input path-str 0))
            (define m (list-argmax Match-score (list m-path m-name)))
            (if (Match-fail? m) #f (cons path m))
        )
        candidates
    ))
    (list-sort pairs > #:key (λ p -> (Match-score (cdr p))) #:cache-keys? #f)
)

(define
    (order-strings
        #:in candidates
        #:according-to user-input
    )
    (define (order-of x)
        (cond
            [(equal? x user-input) 0]
            [(string_prefix? x user-input) 1]
            [else 2]
        ))
    (define (comes-before? x y)
        (if (< (order-of x) (order-of y))
            #t
            #f
        )
    )
    (sort candidates comes-before?)
)

(define
    (find-completions
        #:for input
        #:in strings
    )
    (order-strings #:in strings #:according-to input)
)
