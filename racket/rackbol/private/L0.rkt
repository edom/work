#lang s-exp "base.rkt"

(provide [all-defined-out])

(require
    db
    racklog
    [only-in web-server/servlet
        response/xexpr
    ]
    [only-in web-server/servlet-env
        serve/servlet
    ]
    "ontology.rkt"
)

;;  TODO
;;  https://docs.racket-lang.org/continue/index.html
;;  https://docs.racket-lang.org/db/using-db.html

;;  Automatic per-thread magic that is supposed to "just work",
;;  provided that no threads are killed.
;;
;;  Question: Does a custodian still work if its thread is killed instead of
;;  if its thread finishes normally?

(define (make_virtual_connection storage)
    (define host (->string (GET HOST OF storage)))
    (define port (GET PORT OF storage))
    (define catalog (->string (GET CATALOG OF storage)))
    (define user (->string (GET USER OF storage)))
    (define password (->string (GET PASSWORD OF storage)))
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

;;  (%table Catalog Schema Table)
;;  (%column Catalog Schema Table Column Type)
(define %table %empty-rel)
(define %column %empty-rel)

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

(printf "-------------------- tables~n")
(for ([(catalog schema table)
        (in-query connection
            "SELECT table_catalog, table_schema, table_name
            FROM information_schema.tables
            WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
            LIMIT 10"
        )
    ])
    (printf "~a  ~a  ~a~n" catalog schema table)
    (%assert-after! %table () ((catalog schema table)))
)

(printf "-------------------- columns~n")
(define columns
    (for/list ([(catalog schema table name type)
            (in-query connection
                "SELECT table_catalog, table_schema, table_name, column_name, data_type
                FROM information_schema.columns
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                LIMIT 10"
            )
        ])
        (printf "~a  ~a  ~a  ~a  ~a~n" catalog schema table name type)
        (%assert-after! %column () ((catalog schema table name type)))
        (NEW Column WITH catalog schema table name type)
    ))

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
