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
            #'(#%module-begin
                (provide (all-defined-out))
                (require rackbol/private/L0)
                (define ELEMENTS (list Element ...))
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
