#lang racket

;;  --------------------    A business-oriented language, or a CRUD-Web-application-oriented language.

(module reader syntax/module-reader rackbol)

(provide [except-out (all-from-out racket)
    #%module-begin
])
(provide [rename-out
    (k_module_begin #%module-begin)
])

;;  2019-09-25
;;  This macro is unhygienic because I found it too hard to make it hygienic.
(define-syntax (k_module_begin stx)
    (syntax-case stx ()
        [(_ Body ...)
            (with-syntax (
                    ;;  Model.
                    [M1 #`(module L1 racket
                            (require rackbol/private/L2)
                            (provide (all-from-out rackbol/private/L2))
                            (provide (all-defined-out))
                            ;;  2019-09-25
                            ;;  Why does syntax/strip-context not work?
                            (define ELEMENTS (list #,@(syntax->datum #'(Body ...))))
                    )]
                    ;;  Program generated from model.
                    [M0 `(module L0 racket
                        ;;  2019-09-25
                        ;;  Why does this "require" run L1 twice?
                        (require [for-syntax (submod ".." L1)])
                        (provide (all-defined-out))
                        (define-syntax (GENERATE stx)
                            (with-syntax (
                                    ;;  2019-09-25
                                    ;;  Racket will raise an incomprehensible syntax error blaming this ellipsis
                                    ;;  if M0 is defined with quasisyntax instead of quasiquote.
                                    [(Define ...)
                                        (for/list ([element (in-list ELEMENTS)])
                                            `(define ,(Definition-id element) ,(Definition-value element))
                                        )
                                    ]
                                )
                                #'(begin
                                    Define
                                    ...
                                )
                            )
                        )
                        (GENERATE)

                        (require [only-in web-server/servlet
                            response/xexpr
                        ])
                        (require [only-in web-server/servlet-env
                            serve/servlet
                        ])

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
                                        (p ,(~a counter))
                                    )
                                )
                            )
                        )
                        (define static_path
                            (list (build-path (collection-path "rackbol" "private") "static")))
                        (printf "static_path = ~v~n" static_path)
                        (serve/servlet start
                            #:listen-ip "127.0.0.1"
                            #:port 8080
                            #:servlet-path "/"
                            #:extra-files-paths static_path
                            #:log-file "request.log"
                            #:stateless? #t
                            #:command-line? #t
                            #:launch-browser? #f
                            #:banner? #t
                        )
                    )]
                )
                #'(#%module-begin
                    M1
                    M0
                    (require (submod 'L0))
                )
            )
        ]))
