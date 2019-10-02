#lang racket

;;  --------------------    A business-oriented language, or a CRUD-Web-application-oriented language.

(module reader syntax/module-reader rackbol)

(provide [except-out (all-from-out racket)
    #%module-begin
])
(provide [rename-out
    (k_module_begin #%module-begin)
])
(require rackbol/private/L2)
(provide (all-from-out rackbol/private/L2))

(define-syntax (k_module_begin stx)
    (syntax-case stx ()
        [(_ Element ...)
            #`(#%module-begin
                (provide (all-defined-out))
                (require rackbol/private/L0)
                ;;  Problem: This does not work with XREPL ,enter (Racket enter!)
                ;;  because Racket macro expansion renames ELEMENTS to ELEMENTS.1
                ;;  and typing both in the REPL causes undefined identifier error.
                (define ELEMENTS (list Element ...))
                (define STORAGES
                    [filter-map
                        [lambda (element)
                            (define value (Definition-value element))
                            (define type (TYPE OF value))
                            (if [and (equal? 'Storage type)
                                     (equal? 'postgresql (GET TYPE OF value))]
                                value
                                #f)]
                        ELEMENTS
                    ]
                )
                (define CONNECTION_TABLE
                    (make-hash (map
                        (lambda (storage)
                            (cons (GET id OF storage) (make_virtual_connection storage)))
                        STORAGES)))
                (define SERVERS
                    [filter-map
                        [lambda (element)
                            (define value (Definition-value element))
                            (define type (TYPE OF value))
                            (if [and (equal? 'Server type)
                                     (equal? 'HTTP (GET PROTOCOL OF value))]
                                value
                                #f)]
                        ELEMENTS
                    ])
                (define (main . args)
                    (define SERVER_THREADS (map start_web_server_thread SERVERS))
                    (for-each thread-wait SERVER_THREADS)
                )
            )
        ]))
