#lang s-exp "base.rkt"

(provide
    sql-escape-id
    sql-escape-string
)

;;  XXX: Is this enough?
;;
;;  Other name candidates: sql-quote-id.

(define (sql-escape-id id)
    (define raw
        (cond
            [(symbol? id) (symbol->string id)]
            [(string? id) id]
            [else (error 'sql-escape-id "expecting a symbol or a string instead of: ~a" id)]))
    (define a (string-replace raw "\\" "\\\\"))
    (define b (string-replace a "\"" "\\\""))
    (string-append "\"" b "\"")
)

;;  XXX: Is this correct?

(define (sql-escape-string raw)
    (define a (string-replace raw "\\" "\\\\"))
    (define b (string-replace a "'" "\\'"))
    (string-append "'" b "'")
)
