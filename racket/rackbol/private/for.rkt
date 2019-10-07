#lang s-exp "base.rkt"

(require
    (for-syntax syntax/stx)
    racket/string
    db
)

(provide
    FOR
)

(define-syntax-parser FOR
    #:datum-literals (EACH ROW IN DO)

    ;;  Do not nest FOR statements.
    ;;  See the "N+1 problem".

    [   (For EACH ROW IN table:TABLE-Clause$ DO body ...)
        (with-syntax (
                [   (t_c ...)
                    (stx-map
                        (lambda (c) (format-id #'For "~a.~a" #'table.alias c))
                        #'(table.column ...)
                    )]
                [Connection #'table.connection-id]
            )
            ;;  2019-10-07: "in-query" is the only way to use cursors in the db library.
            #'(let* (
                    [column-list    (~a (sql-escape-id 'table.column) ... #:separator ", ")]
                    [table-qname    (~a (sql-escape-id 'table.schema) "." (sql-escape-id 'table.name))]
                    [statement      (~a 'SELECT column-list 'FROM table-qname #:separator " ")]
                )
                ;;  XXX: #:fetch should depend on table size.
                ;;  "in-query" can only be called inside "call-with-transaction".
                (with-transaction Connection
                    (for ([(t_c ...) (in-query Connection statement #:fetch 1024)])
                        body ...
                    ))))]
)

(define-syntax-rule (with-transaction Connection Body ...)
    (call-with-transaction Connection (lambda () Body ...))
)

;;  XXX: Is this enough?

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
