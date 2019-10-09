#lang s-exp "../web.rkt"

(provide (all-defined-out))

;;  TODO
;;  https://docs.racket-lang.org/continue/index.html
;;  https://docs.racket-lang.org/db/using-db.html

#|
(DEFINE TABLE information_schema:tables
    SCHEMA information_schema
    COLUMN table_catalog
    COLUMN table_schema
    COLUMN table_name
)
;;  or
(read_schema_from connection)
;;  then
(struct SQL_Select (columns from where))
(SELECT
    table_catalog AS catalog
    table_schema AS schema
    table_name AS name
    FROM information_schema:tables
    WHERE table_schema NOT IN [pg_catalog information_schema]
    DO
    (printf "~v~n" (list catalog schema name))
)
|#

(define-syntax-rule (with-semaphore Sem Body ...)
    (call-with-semaphore Sem (lambda () Body ...))
)

(define current-request (make-parameter #f))

(define (start_web_server_thread model)
    (define ID (GET id OF model))
    (define ADDRESS (GET ADDRESS OF model))
    (define PORT (GET PORT OF model))
    (define s_start_time (format-date/iso (current-date)))
    (define sem (make-semaphore 1))
    ;;  Problem: This request_count can grow without bounds,
    ;;  although it takes a very long time for it to grow big enough to impact the running program.
    ;;  We assume that the program is restarted periodically.
    (define request_count 0)
    (define (get_count)         (with-semaphore sem request_count))
    (define (increment_count)   (with-semaphore sem (set! request_count (+ request_count 1))))
    (define (furnish xexpr_list)
        (response/xexpr
            ;;  https://lists.racket-lang.org/users/archive/2011-November/049394.html
            #:preamble #"<!doctype html>"
            `(html
                (head
                    (title "Test")
                    (link ((rel "stylesheet") (type "text/css") (href "/style.css")))
                )
                (body
                    (main ,@xexpr_list)
                    (footer
                        (p "Current server time: " ,(format-date/iso (current-date)))
                        (p "Server start time: " ,s_start_time)
                        (p "Uptime: ")
                        (p "Request count: " ,(~a (get_count)))
                    )
                )
            )
        )
    )
    (define (response/furnish xexpr_list)
        (response/xexpr
            #:preamble #"<!doctype html>"
            (furnish xexpr_list)))
    (define (not_found)
        (response/xexpr
            #:code 404
            #:preamble #"<!doctype html>"
            (furnish
                `[(h1 "Not found")]
            )))
    (define routes
        (hash
            (cons "GET" "/") (λ => response/furnish `[(h1 "Home")])
            (cons "GET" "/test") (λ => response/furnish `[(h1 "Test")])
        ))
    (define (start request)
        (increment_count)
        (define method "GET")
        (define url "/")
        (define handle (hash-ref routes (cons method url) (const not_found)))
        (parameterize ([current-request request])
            (handle)
        )
    )
    (define static_path [list (build-path (collection-path "rackbol" "private") "static")])
    (printf "~a: static_path = ~v~n" ID static_path)
    (printf "~a: Listening on ~a:~a~n" ID ADDRESS PORT)
    (thread [lambda ()
        (serve/servlet start
            #:listen-ip ADDRESS
            #:port PORT
            #:servlet-path "/"
            #:extra-files-paths static_path
            #:log-file "request.log"
            #:stateless? #t
            #:command-line? #t
            #:launch-browser? #f
            #:banner? #f
        )])
)

;;  --------------------    Logic programming.

(require "../private/logic.rkt")

(define %father_child
    (%rel ()
        [('f1 'c1)]
        [('f2 'c2)]
    ))

(define %mother_child
    (%rel ()
        [('m1 'c1)]
        [('m2 'c2)]
    ))

(define (logic)
    (%for-each F M C #:satisfying
        (%father_child F C)
        (%mother_child M C)
    #:do
        (printf "~a ~a ~a~n" F M C)
    )

    (%FOR-EACH F M C SATISFYING
        (%father_child F C)
        (%mother_child M C)
    DO
        (printf "~a ~a ~a~n" F M C)
    )
)

;;  --------------------    Import.

(define (show-tables)
    (FOR EACH ROW IN TABLE pg_1 information_schema tables
        AS t (table_catalog table_schema table_name)
        DO  (printf "~a ~a ~a~n" t.table_catalog t.table_schema t.table_name)
    )
)

(define (show-columns)
    (FOR EACH ROW IN TABLE pg_1 information_schema columns
        AS c (table_catalog table_schema table_name column_name data_type)
        DO  (printf "~a ~a ~a ~a ~a~n" c.table_catalog c.table_schema c.table_name c.column_name c.data_type)
    )
)

(DEFINE STORAGE pg_1
    TYPE postgresql
    HOST localhost
    PORT 5432
    CATALOG test
    USER test
    PASSWORD test
)

(define (import)
    (define table_order_column (make-hash))
    ;;  TODO: Parameterize pg_1
    ;;  TODO: LEFT JOIN
    (FOR EACH ROW IN pg_1
        (SELECT table_catalog table_schema table_name
            FROM information_schema.tables
            WHERE (NOT-IN table_schema ["pg_catalog" "information_schema"])
        )
        DO  (define key (list table_catalog table_schema table_name))
            (hash-set! table_order_column key (make-hash))
    )
    (FOR EACH ROW IN pg_1
        (SELECT table_catalog table_schema table_name column_name ordinal_position
            FROM information_schema.columns
            WHERE (NOT-IN table_schema ["pg_catalog" "information_schema"])
        )
        DO  (define key (list table_catalog table_schema table_name))
            (hash-set! (hash-ref table_order_column key) ordinal_position column_name)
    )
    (pretty-print table_order_column)
    (define tables
        (for/list ([(cat-sch-tab order_column) (in-hash table_order_column)])
            (match cat-sch-tab
                [(list catalog schema name)
                    (list
                        catalog schema name
                        (hash-map order_column (λ k v -> v) #t)
                    )
                ]
            )
        )
    )
    (pretty-print tables)
)

(import)
