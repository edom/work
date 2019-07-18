#lang stc-racket

;;  TODO
;;  https://docs.racket-lang.org/continue/index.html

(require db)

;;  https://docs.racket-lang.org/db/using-db.html

;;  Automatic per-thread magic that is supposed to "just work",
;;  provided that no threads are killed.

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

(for (
        ((catalog schema name)
            (in-query connection
                "SELECT table_catalog, table_schema, table_name
                FROM information_schema.tables
                WHERE table_schema NOT IN ('pg_catalog', 'information_schema')
                LIMIT 10"
            )
        )
    )
    (printf "~a  ~a  ~a~n" catalog schema name)
)
