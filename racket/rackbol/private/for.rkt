#lang s-exp "base.rkt"

(require
    "sql.rkt"
    "sql-syntax.rkt"
)

(provide
    FOR
)

;;  Do not nest FOR statements.
;;  See the "N+1 problem".
;;
;;  FIXME: This confuses Racket so much that
;;  it produces an unhelpful error message with unclear backtrace,
;;  complaining that "storage-virtual-connection" is undefined,
;;  even though the real problem is that "storage" is undefined.
;;
;;  TODO: Make #:fetch depend on table size.
;;
;;  XXX: 2019-10-07:
;;  "in-query" can only be called inside "call-with-transaction".
;;  "in-query" is the only way to use cursors in the db library.

(define-syntax-parser FOR
    #:datum-literals (EACH ROW IN DO)
    ;;  Deprecated.
    [   (For EACH ROW IN table:TABLE-Clause$ DO body ...)
        (with-syntax (
                [   (t_c ...)
                    (stx-map
                        (lambda (c) (format-id #'For "~a.~a" #'table.alias c))
                        #'(table.column ...)
                    )]
                ;;  FIXME: Contract violation:
                ;;  Connection should be a virtual-connection-id but storage is a storage-id.
                [Connection #'table.storage]
            )
            #'(let* (
                    [column-list    (~a (sql-escape-id 'table.column) ... #:separator ", ")]
                    [table-qname    (~a (sql-escape-id 'table.schema) "." (sql-escape-id 'table.name))]
                    [statement      (~a 'SELECT column-list 'FROM table-qname #:separator " ")]
                )
                (with-transaction Connection
                    (for ([(t_c ...) (in-query Connection statement #:fetch 1024)])
                        body ...
                    ))))
    ]
    ;;  XXX: How do we infer connection ID from storage ID?
    [   (For EACH ROW IN Connection:id select:SELECT$ DO body ...)
        (with-syntax (
                [Vars #'select.column-aliases]
                [Statement #'select.string]
            )
            #'(with-transaction Connection
                (for ([Vars (in-query Connection Statement #:fetch 1024)])
                    body ...
                )))
    ]
)

;;  FIXME: Move

(require db)

(define-syntax-rule (with-transaction Connection Body ...)
    (call-with-transaction Connection (lambda () Body ...))
)
