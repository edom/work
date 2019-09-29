#lang s-exp "0/_out.rkt"

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

(define (start_web_server_thread model)
    (define ID (GET id OF model))
    (define ADDRESS (GET ADDRESS OF model))
    (define PORT (GET PORT OF model))
    (define s_start_time (format-date/iso (current-date)))
    (define request_count 0) ;; FIXME synchronize
    (define (start request)
        ;;  Is this thread-safe?
        ;;  https://docs.racket-lang.org/reference/threads.html
        (set! request_count (+ request_count 1))
        (response/xexpr
            `(html
                (head
                    (title "Test")
                    (link ((rel "stylesheet") (type "text/css") (href "/style.css")))
                )
                (body
                    (main
                        (h1 "Test")
                    )
                    (footer
                        (p "Current server time: " ,(format-date/iso (current-date)))
                        (p "Server start time: " ,s_start_time)
                        (p "Uptime: ")
                        (p "Request count: " ,(~a request_count))
                    )
                )
            )
        )
    )
    (define static_path [list (build-path (collection-path "rackbol" "private") "static")])
    (printf "~a: static_path = ~v~n" ID static_path)
    (printf "~a: Will listen on ~a:~a~n" ID ADDRESS PORT)
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
