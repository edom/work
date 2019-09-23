#lang stc-racket

;;  TODO
;;  https://docs.racket-lang.org/continue/index.html

(require db)

;;  https://docs.racket-lang.org/db/using-db.html

;;  Automatic per-thread magic that is supposed to "just work",
;;  provided that no threads are killed.
;;
;;  Question: Does a custodian still work if its thread is killed instead of
;;  if its thread finishes normally?

(define connection
    (virtual-connection
        (connection-pool
            (lambda ()
                (postgresql-connect
                    #:user "test"
                    #:database "test"
                    #:password "test"
                    #:server "localhost"
                )
            )
            #:max-connections 8
            #:max-idle-connections 8
        )
    )
)

(define-struct-2 Column (catalog schema table name type) #:prefab)

;;  (%table Catalog Schema Table)
;;  (%column Catalog Schema Table Column Type)
(define %table %empty-rel)
(define %column %empty-rel)

(printf "-------------------- tables~n")
(for (
        ((catalog schema table)
            (in-query connection
                "SELECT table_catalog, table_schema, table_name
                FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                LIMIT 10"
            )
        )
    )
    (printf "~a  ~a  ~a~n" catalog schema table)
    (%assertz! %table () ((catalog schema table)))
)

(printf "-------------------- columns~n")
(define columns
    (for/list (
            ((catalog schema table name type)
                (in-query connection
                    "SELECT table_catalog, table_schema, table_name, column_name, data_type
                    FROM information_schema.columns
                    WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                    LIMIT 10"
                )
            )
        )
        (printf "~a  ~a  ~a  ~a  ~a~n" catalog schema table name type)
        (%assertz! %column () ((catalog schema table name type)))
        (Column catalog schema table name type)
    )
)

(printf "-------------------- %table~n")
(apply values
    (%find-all (Catalog Schema Table)
        (%table Catalog Schema Table))
)
(printf "-------------------- %column~n")
(apply values
    (%find-all (Catalog Schema Table Column Type)
        (%column Catalog Schema Table Column Type)
    )
)

(require+provide web-server/servlet
    response/xexpr
)
(require+provide web-server/servlet-env
    serve/servlet
)

(define counter 0)

(define (start request)
    (set! counter (+ counter 1))
    (response/xexpr
        `(html
            (head
                (title "Test")
                (link ((rel "stylesheet") (type "text/css") (href "/style.css")))
            )
            (body
                (h1 "Test")
                ,(~a columns)
                (p ,(~a counter))
            )
        )
    )
)

(serve/servlet start
    #:listen-ip "127.0.0.1"
    #:port 8080
    #:servlet-path "/"
    #:extra-files-paths (list "static")
    #:log-file "request.log"
    #:stateless? #t
    #:command-line? #t
    #:launch-browser? #f
    #:banner? #t
)
